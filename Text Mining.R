#'Setup
remove(list = ls())
#install.packages("remotes")
#install.packages("textcat")
#install.packages("stopwords")
#'Install Packages 
library(dplyr)  
library(tidytext)  
library(ggplot2)
library(reshape2) 
library(wordcloud)
library(twitteR)
library(ldatuning)
library(topicmodels)
library(tm)
library(stringr)
library(RColorBrewer)
remotes::install_github("sebastiansauer/pradadata")
library(pradadata)
library(textcat)
library(stopwords)

#'Login to twitter developer
options(httr_oauth_cache=T)
my_api_key             <- "CeAIf6W3DcwZ0gGe3UHihhZgZ"
my_api_secret          <- "muhNVlMH6uBVnCle1zdlmIdFkqIH94NMwOGhYMECamDzHWG4jw"
my_access_token        <- "1042754223258193921-S4KcAhqAkmie0zp9XIArY3hM6UTgXQ"
my_access_token_secret <- "5YCHFa6MoFhY3FcuKIJmQiJt7VFD0fHxnjwWxomzIDMZI"
setup_twitter_oauth(my_api_key,my_api_secret,my_access_token,my_access_token_secret)

#'
#'  X Twitter Data (generic name, just replace user in the first line and in the graph title)
#'  
tweets <- userTimeline("@heutejournal", n=3200, includeRts=F) # userTimline to get the n first tweets 
tweets_df <- twListToDF(tweets) # convert it into a dataframe

#' Only keeping date after February 2020
tweets_df$created <- as.Date(tweets_df$created, "%d/%m/%Y") # add this column to the dataset 
tweets_df <- tweets_df %>% filter(created >="2020-02-01")

#' Count the number of mentions per day 
mentions_per_day <- count(tweets_df, created)

#' Plot the number of mentions per day to see if the amount of tweet increased with the corona spread 
jpeg("Heute Journal per Day.jpg", width = 1500)
ggplot(mentions_per_day, aes(created, n)) + geom_line()
dev.off()

Today <- tweets_df

##' Sentiment Analysis 
##' 
#' Convert the initial data into Tidy Text Format 

tweets_df$id <- seq(1, nrow(tweets_df))
tweets_unnested <- unnest_tokens(tweets_df, word, text)

#' Remove stopwords and strange words 
stop_german <- data.frame(word = stopwords::stopwords("de"), stringsAsFactors = FALSE)
View(stop_german)
tweets_unnested <- tweets_unnested[!tweets_unnested$word %in% stop_german$word, ]
tweets_unnested <- tweets_unnested[str_detect(tweets_unnested$word, "^[a-z']+$"),]
nrow(tweets_unnested)
sentiws 

#' Pick token polarity from AFINN Lexicon 
sentiws <- as.data.frame(sentiws)
sentiws$value <- sentiws$value*5 # Attention only run it once or clear your environment 
sentiws <- as_tibble(sentiws)
tweets_unnested <- inner_join(tweets_unnested, sentiws, by = "word") 
View(tweets_unnested)


#' Compute average polarity per tweet 
tweet_polarity <- aggregate(value ~ id, tweets_unnested, mean)
tweets_df <- inner_join(tweets_df, tweet_polarity, by ="id") 

#' Compute the average sentiment per date 
date_sentiment <- aggregate(value ~ created, tweets_df, mean)

#' Plot the average sentiment per date 
jpeg("USA Today Health.jpg", width = 1500)
ggplot(date_sentiment, aes(x = created, y = value)) +
  geom_smooth(method="loess", size = 1, se=T, span = .5) +
  geom_hline(yintercept = 0, color = "grey") +
  ylab("Avg. Sentiment") +
  xlab("Date") +
  ggtitle("USA Today Health Sentiment Evolution")
dev.off()

#Wordcloud
jpeg("USA Today Health Wordcloud.jpg", width = 1500)
tweets_unnested %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
dev.off()

##Topic Analysis
#' Extract the actual text and convert this corpus into a Term Document Matrix "borisjohnson"
#' 
tweetsCorpus <- Corpus(VectorSource(Maitlis$text))
tweetsCorpus

changeStrangeApostrophes <- content_transformer(function(x){return (gsub("'", "'", x))})
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
#tweetsCorpus <- tm_map(tweetsCorpus, content_transformer(tolower))
#Remove stopwords
tweetsCorpus <-  tm_map(tweetsCorpus, removeWords, stopwords("english")) #when specifying function parameters, they are specified as arguments after the function
otherStopWords <- c("this","the",".", "ca", "your", "just", "even", "new", "use", "right", "one", "today", "time", "pick", "here", "will", "give", "otherwhise", "can", "here", "part", "look", "part") #other stopwords, they were defined after inspecting the results without them. You can try to execute this line, analyze the results and check for words that provide no meaning. You will find all of those.
tweetsCorpus <- tm_map(tweetsCorpus, removeWords, otherStopWords)
#Stem the document
tweetsCorpus <- tm_map(tweetsCorpus, stemDocument, language = "english")

#Wordcloud
jpeg("Emily Maitlis General Wordcloud.jpg", width = 1500)
wordcloud(tweetsCorpus, colors=brewer.pal(8,"Dark2"), max.words = 200, min.freq = 3)
dev.off()

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

tweetsLDA <- LDA(tweetsDTM, k = 3, control = list(seed = 100))

tidyLda <- tidy(tweetsLDA)

topTerms <- tidyLda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

jpeg("Emily Maitlis Topics.jpg", width = 1500)
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

##Country sentiment analysis
country_df <- rbind(Trump, Oprah, Today)
US <- country_df

#' Convert the initial data into Tidy Text Format 
country_df$id <- seq(1, nrow(country_df))
tweets_unnested <- unnest_tokens(country_df, word, text)

#' Remove stopwords and strange words 
tweets_unnested <- tweets_unnested[!tweets_unnested$word %in% stop_words$word, ]
tweets_unnested <- tweets_unnested[str_detect(tweets_unnested$word, "^[a-z']+$"),]

#' Pick token polarity from AFINN Lexicon 
tweets_unnested <- inner_join(tweets_unnested, get_sentiments("afinn"), by = "word")

#' Compute average polarity per tweet 
tweet_polarity <- aggregate(value ~ id, tweets_unnested, mean)
country_df <- inner_join(country_df, tweet_polarity, by ="id") 

#' Compute the average sentiment per date 
date_sentiment <- aggregate(value ~ created, country_df, mean)

#' Plot the average sentiment per date 
jpeg("US.jpg", width = 1500)
ggplot(date_sentiment, aes(x = created, y = value)) +
  geom_smooth(method="loess", size = 1, se=T, span = .5) +
  geom_hline(yintercept = 0, color = "grey") +
  ylab("Avg. Sentiment") +
  xlab("Date") +
  ggtitle("US Sentiment Evolution")
dev.off()

#Wordcloud
jpeg("US Wordcloud.jpg", width = 1500)
tweets_unnested %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)
dev.off()
