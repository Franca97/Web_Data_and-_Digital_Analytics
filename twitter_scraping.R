#'Setup
#'Install Packages 

library(dplyr) #' for sentiment analysis 
library(tidytext) #' for sentiment analysis 
library(ggplot2)
install.packages("twitteR")
library(twitteR)
library(ldatuning)
library(topicmodels)
library(tm)

#'Login to twitter developer
options(httr_oauth_cache=T)
my_api_key             <- "DdLxsLaCKmd7WTJx478P0E10j"
my_api_secret          <- "NLrFfUgkfJVrs3UXoSSs4KGrzYFBu4BcjnpopIDM4az2EX3I4a"
my_access_token        <- "1252197116849672192-JkvDiU5dOe7WiLCn4BTXRPWQ3KpIoC"
my_access_token_secret <- "91oR1BrHclWQZZUygvZOeQZYb4Rml7zkvk4HtNnZtqYDU"
setup_twitter_oauth(my_api_key,my_api_secret,my_access_token,my_access_token_secret)

#' Scrape Twitter Data 
#' 
tweets_emmawatson <- userTimeline("EmmaWatson", n=250)
tweets_df_emmawatson <- twListToDF(tweets_emmawatson)
dim(tweets_df_emmawatson)

head(tweets_df_emmawatson)

#extract the actual text and convert this corpus into a Term Document Matrix emmawatson

library(tm)

corpus <- Corpus(VectorSource(tweets_df_emmawatson$text))
tdm_emmawatson <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stopwords=TRUE, stemming=TRUE))
tdm_emmawatson
gi
#visualize data emmawatson

library(wordcloud)

m1 <- as.matrix(tdm_emmawatson)
v1 <- sort(rowSums(m1), decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)
head(d1, 15)

wordcloud(words = d1$word, freq = d1$freq, min.freq =2 ,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

--- 


#'
#'  Boris Johnson Twitter Data
#'  
tweets_bojo <- userTimeline("BorisJohnson", n=1000) # userTimline to get the n first tweets 
tweets_df_bojo <- twListToDF(tweets_bojo) # convert it into a dataframe
dim(tweets_df_bojo) 
tweets_bojo
head(tweets_df_bojo)
View(tweets_df_bojo)

#' Checking Twitter Data 
Dates <- format(as.POSIXct(strptime(tweets_df_bojo$created, format = "%Y-%m-%d %H:%M:%S",tz="")),format = "%Y-%m-%d") #' creating a date column from the created column 
tweets_df_bojo$Dates <- Dates # add this column to the dataset 

#' Count the number of mentions per day 
mentions_per_day_bojo <- count(tweets_df_bojo, Dates)

#' Plot the number of mentions per day to see if the amount of tweet increased with the corona spread 
ggplot(mentions_per_day_bojo, aes(Dates, n)) + geom_point() 
ggplot(mentions_per_day_bojo[mentions_per_day_bojo$Dates > "2020-05-01",], aes(Dates, n)) + geom_point() # adjust date 


#' Extract the actual text and convert this corpus into a Term Document Matrix "borisjohnson"
#' 
corpus_bojo <- Corpus(VectorSource(tweets_df_bojo$text)) # extract actual text and store it in a corpus

#' Pre-process the corpus 

changeStrangeApostrophes <- content_transformer(function(x) {return (gsub('’', "'", x))})
corpus_bojo <- tm_map(corpus_bojo, changeStrangeApostrophes)

#' Remove URLs 

removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
corpus_bojo <- tm_map(corpus_bojo, removeURL)


#' Remove dashes and quotation marks 
remove <- content_transformer(function(x, pattern) {return (gsub(pattern, '', x))})
corpus_bojo <- tm_map(corpus_bojo, remove, '—')
corpus_bojo <- tm_map(corpus_bojo, remove, '-')
corpus_bojo <- tm_map(corpus_bojo, remove, '€')
corpus_bojo <- tm_map(corpus_bojo, remove, "'")
corpus_bojo <- tm_map(corpus_bojo, remove, '“')
corpus_bojo <- tm_map(corpus_bojo, remove, '”')
corpus_bojo <- tm_map(corpus_bojo, remove, "/")
corpus_bojo <- tm_map(corpus_bojo, remove, "@")
corpus_bojo <- tm_map(corpus_bojo, remove, "\\|")

#' Remove Numbers 
corpus_bojo <- tm_map(corpus_bojo, removeNumbers)
#' Remove punctuation
corpus_bojo <- tm_map(corpus_bojo, removePunctuation)
#' Stip whitespaces
corpus_bojo <- tm_map(corpus_bojo, stripWhitespace)
#' Text to lowercase
corpus_bojo <- tm_map(corpus_bojo, content_transformer(tolower))
#' Remove stopwords
corpus_bojo <-  tm_map(corpus_bojo, removeWords, stopwords("english"))
#' Stem the document 
corpus_bojo <- tm_map(corpus_bojo, stemDocument, language = "english")

#' Checking with wordcloud 
wordcloud(corpus_bojo, colors=brewer.pal(8,"Dark2"), max.words = 200, min.freq = 3)

#' Create a Document Term Matrix 

dtm_bojo <- TermDocumentMatrix(corpus_bojo) 
dtm_bojo

#' Assign some identifier to Document names in the DTM 
dtm_bojo$dimnames$Docs <- tweets_df_bojo$text

#' Remove zero-entries from the DTM 
dtm_bojo <- dtm_bojo[unique(dtm_bojo$i), ] # pick only non zero documents 
dtm_bojo # has not changed anything?!

#' Topic modeling 
#' Find the optimal number of topics 

topic_number_bojo <- FindTopicsNumber(
  dtm_bojo, #DTM object
  topics = seq(from = 2, to = 15, by = 1), #Number of topics to test, i.e. between 2 and 15
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), #Metrics to check
  control = list(seed = 100), #Random seed
  verbose = TRUE
  
)

#' Plot topic Models 
#' 
FindTopicsNumber_plot(topic_number_bojo)  # six or seven topics 

#' Run an LDA model 
bojo_tweets_LDA <- LDA(dtm_bojo, k = 4, control = list(seed = 100))

#' Get the Top 10 Terms for each topic 
bojo_tidyLda <- tidy(bojo_tweets_LDA)

bojo_topTerms <- bojo_tidyLda %>% 
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)


bojo_topTerms %>% 
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
       x = NULL, y = expression(beta)) +  #set title and y label. 
  facet_wrap(~ topic, ncol = 3, scales = "free") #divide by topic

##' Sentiment Analysis 
##' 
#' Convert the initial data into Tidy Text Format 
#' 
library(stringr)

tweets_df_bojo$id <- seq(1, nrow(tweets_df_bojo))
tweets_unnested_bojo <- unnest_tokens(tweets_df_bojo, word, text)

#' Remove stopwords and strange words 
tweets_unnested_bojo <- tweets_unnested_bojo[!tweets_unnested_bojo$word %in% stop_words$word, ]
tweets_unnested_bojo <- tweets_unnested_bojo[str_detect(tweets_unnested_bojo$word, "^[a-z']+$"),]

#' Pick token polarity from AFINN Lexicon 
tweets_unnested_bojo <- inner_join(tweets_unnested_bojo, get_sentiments("afinn"), by = "word")

#' Compute average polarity per tweet 
tweet_polarity_bojo <- aggregate(value ~ id, tweets_unnested_bojo, mean)
tweets_df_bojo <- inner_join(tweets_df_bojo, tweet_polarity_bojo, by ="id") 

#' Compute the average sentiment per date 

date_sentiment_bojo <- aggregate(value ~ Dates, tweets_df_bojo, mean)

#' Plot the average sentiment per date 

ggplot(date_sentiment_bojo, aes(x = Dates, y = value)) +
  geom_smooth(method="loess", size = 1, se=T, span = .5) +
  geom_hline(yintercept = 0, color = "grey") +
  ylab("Avg. Sentiment") +
  xlab("Date") +
  ggtitle("Sentiment Corona Mentions")



#' Wordcloud: to take a first look at the most frequency words and visualize it with a worldcloud 
#' 
m2 <- as.matrix(dtm_bojo)
v2 <- sort(rowSums(m2), decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
head(d2, 15)

wordcloud(words = d2$word, freq = d2$freq, min.freq =2 ,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



---


#scrape data theguardian

tweets_theguardian <- userTimeline("@guardian", n=250)
tweets_df_theguardian <- twListToDF(tweets_theguardian)
dim(tweets_df_theguardian)
tweets_theguardian
head(tweets_df_theguardian)

#extract the actual text and convert this corpus into a Term Document Matrix theguardian


corpus <- Corpus(VectorSource(tweets_df_theguardian$text))
tdm_theguardian <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stopwords=TRUE, stemming=TRUE))
tdm_theguardian


#scrape data donaldtrump

tweets_donaldtrump <- userTimeline("@realDonaldTrump", n=250)
tweets_df_donaldtrump <- twListToDF(tweets_donaldtrump)
dim(tweets_df_donaldtrump)
tweets_donaldtrump
head(tweets_df_donaldtrump)



#scrape data ellen degeneres
tweets_ellendegeneres <- userTimeline("@TheEllenShow", n=250)
tweets_df_ellendegeneres <- twListToDF(tweets_ellendegeneres)
dim(tweets_df_ellendegeneres)
tweets_ellendegeneres
head(tweets_df_ellendegeneres)

#scrape data new york times
tweets_newyorktimes <- userTimeline("@nytimes", n=250)
tweets_df_newyorktimes <- twListToDF(tweets_newyorktimes)
dim(tweets_df_newyorktimes)
tweets_newyorktimes
head(tweets_df_newyorktimes)



#scrape data Alain Berset
tweets_alainberset <- userTimeline("@alain_berset", n=250)
tweets_df_alainberset <- twListToDF(tweets_alainberset)
dim(tweets_df_alainberset)
tweets_alainberset
head(tweets_df_alainberset)


#scrape data Roger Federer
tweets_rogerfederer <- userTimeline("@rogerfederer", n=250)
tweets_df_rogerfederer <- twListToDF(tweets_rogerfederer)
dim(tweets_df_rogerfederer)
tweets_rogerfederer
head(tweets_df_rogerfederer)

#scrape data NZZ
tweets_nzz <- userTimeline("@NZZ", n=250)
tweets_df_nzz <- twListToDF(tweets_nzz)
dim(tweets_df_nzz)
tweets_nzz
head(tweets_df_nzz)
