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
#'  Boris Johnson Twitter Data
#'  
tweets_bojo <- userTimeline("BorisJohnson", n=2500) # userTimline to get the n first tweets 
tweets_df_bojo <- twListToDF(tweets_bojo) # convert it into a dataframe

#' Checking Twitter Data 
tweets_df_bojo$created <- as.Date(tweets_df_bojo$created, "%d/%m/%Y") # add this column to the dataset 
tweets_df_bojo <- tweets_df_bojo %>% filter(created >="2020-02-01") 

#' Count the number of mentions per day 
mentions_per_day_bojo <- count(tweets_df_bojo, created)

#' Plot the number of mentions per day to see if the amount of tweet increased with the corona spread 
ggplot(mentions_per_day_bojo, aes(created, n)) + geom_point() 

#' Extract the actual text and convert this corpus into a Term Document Matrix "borisjohnson"
#' 
tweetsCorpus <- Corpus(VectorSource(tweets_df_bojo$text))
tweetsCorpus

changeStrangeApostrophes <- content_transformer(function(x) {return (gsub(''', "'", x))})
tweetsCorpus <- tm_map(tweetsCorpus, changeStrangeApostrophes)

#Remove URLs
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
tweetsCorpus <- tm_map(tweetsCorpus, removeURL)

#Remove dashes and quotation marks
remove <- content_transformer(function(x, pattern) {return (gsub(pattern, '', x))})
tweetsCorpus <- tm_map(tweetsCorpus, remove, '-')
tweetsCorpus <- tm_map(tweetsCorpus, remove, '-')
tweetsCorpus <- tm_map(tweetsCorpus, remove, '???')
tweetsCorpus <- tm_map(tweetsCorpus, remove, "'")
tweetsCorpus <- tm_map(tweetsCorpus, remove, '"')
tweetsCorpus <- tm_map(tweetsCorpus, remove, '"')
tweetsCorpus <- tm_map(tweetsCorpus, remove, "/")
tweetsCorpus <- tm_map(tweetsCorpus, remove, "@")
tweetsCorpus <- tm_map(tweetsCorpus, remove, "\\|")

#Remove Numbers
tweetsCorpus <- tm_map(tweetsCorpus, removeNumbers)
#Remove punctuation
tweetsCorpus <- tm_map(tweetsCorpus, removePunctuation)
#Stip whitespaces
tweetsCorpus <- tm_map(tweetsCorpus, stripWhitespace)
#Text to lowercase
tweetsCorpus <- tm_map(tweetsCorpus, content_transformer(tolower))
#Remove stopwords
tweetsCorpus <-  tm_map(tweetsCorpus, removeWords, stopwords("english")) #when specifying function parameters, they are specified as arguments after the function
otherStopWords <- c(".", "ca", "your", "just", "even", "new", "use", "right", "one", "today", "time", "pick", "here", "will", "give", "otherwhise", "can", "here", "part", "look", "part") #other stopwords, they were defined after inspecting the results without them. You can try to execute this line, analyze the results and check for words that provide no meaning. You will find all of those.
tweetsCorpus <- tm_map(tweetsCorpus, removeWords, otherStopWords)
#Stem the document
tweetsCorpus <- tm_map(tweetsCorpus, stemDocument, language = "english")

tweetsDTM <- DocumentTermMatrix(tweetsCorpus)
tweetsDTM

tweetsDTM <- tweetsDTM[unique(tweetsDTM$i), ] #pick only non zero-entry documents
tweetsDTM

topicNumberMetrics <- FindTopicsNumber(
  tweetsDTM, #DTM object
  topics = seq(from = 2, to = 15, by = 1), #Number of topics to test, i.e. between 2 and 15
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), #Metrics to check
  control = list(seed = 100), #Random seed
  verbose = TRUE
  )

FindTopicsNumber_plot(topicNumberMetrics)

tweetsLDA <- LDA(tweetsDTM, k = 5, control = list(seed = 100))

tidyLda <- tidy(tweetsLDA)

topTerms <- tidyLda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

jpeg("Topics Boris Johnson.jpg", width = 1500)
topTerms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>% 
  mutate(term = factor(paste(term, topic, sep = "__"),
                       levels = rev(paste(term, topic, sep = "__")))) %>% #previous code is mainly done to order the terms per topic according to their beta
  ggplot(aes(term, beta, fill = as.factor(topic))) + # x axis to term  y to beta and fill them according to the topic number
  geom_col(show.legend = FALSE) + #column graph without legend
  coord_flip() + #flip graphs
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +  #set title annd y label. 
  facet_wrap(~ topic, ncol = 3, scales = "free") #divide by topic
dev.off()
