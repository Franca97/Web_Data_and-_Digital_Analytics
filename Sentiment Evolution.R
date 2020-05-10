#'Setup
remove(list = ls())

#'Install Packages 
library(dplyr) #' for sentiment analysis 
library(tidytext) #' for sentiment analysis 
library(ggplot2)
#install.packages("twitteR")
library(twitteR)
library(ldatuning)
library(topicmodels)
library(tm)
library(stringr)

#'Login to twitter developer
options(httr_oauth_cache=T)
my_api_key             <- "DdLxsLaCKmd7WTJx478P0E10j"
my_api_secret          <- "NLrFfUgkfJVrs3UXoSSs4KGrzYFBu4BcjnpopIDM4az2EX3I4a"
my_access_token        <- "1252197116849672192-JkvDiU5dOe7WiLCn4BTXRPWQ3KpIoC"
my_access_token_secret <- "91oR1BrHclWQZZUygvZOeQZYb4Rml7zkvk4HtNnZtqYDU"
setup_twitter_oauth(my_api_key,my_api_secret,my_access_token,my_access_token_secret)

#'
#'  X Twitter Data (generic name, just replace user in the first line and in the graph title)
#'  
tweets <- userTimeline("@realDonaldTrump", n=2500) # userTimline to get the n first tweets 
tweets_df <- twListToDF(tweets) # convert it into a dataframe


#' Only keeping date after February 2020
tweets_df$created <- as.Date(tweets_df$created, "%d/%m/%Y") # add this column to the dataset 
tweets_df <- tweets_df %>% filter(created >="2020-02-01")

#' Count the number of mentions per day 
mentions_per_day <- count(tweets_df, created)

#' Plot the number of mentions per day to see if the amount of tweet increased with the corona spread 
ggplot(mentions_per_day, aes(created, n)) + geom_point() 

##' Sentiment Analysis 
##' 
#' Convert the initial data into Tidy Text Format 

tweets_df$id <- seq(1, nrow(tweets_df))
tweets_unnested <- unnest_tokens(tweets_df, word, text)

#' Remove stopwords and strange words 
tweets_unnested <- tweets_unnested[!tweets_unnested$word %in% stop_words$word, ]
tweets_unnested <- tweets_unnested[str_detect(tweets_unnested$word, "^[a-z']+$"),]

#' Pick token polarity from AFINN Lexicon 
tweets_unnested <- inner_join(tweets_unnested, get_sentiments("afinn"), by = "word")

#' Compute average polarity per tweet 
tweet_polarity <- aggregate(value ~ id, tweets_unnested, mean)
tweets_df <- inner_join(tweets_df, tweet_polarity, by ="id") 

#' Compute the average sentiment per date 
date_sentiment <- aggregate(value ~ created, tweets_df, mean)

#' Plot the average sentiment per date 
jpeg("Trump.jpg", width = 1500)

ggplot(date_sentiment, aes(x = created, y = value)) +
  geom_smooth(method="loess", size = 1, se=T, span = .5) +
  geom_hline(yintercept = 0, color = "grey") +
  ylab("Avg. Sentiment") +
  xlab("Date") +
  ggtitle("Donald Trump Sentiment Evolution")

dev.off()
