#login to twitter developer

library(twitteR)
options(httr_oauth_cache=T)
my_api_key             <- "DdLxsLaCKmd7WTJx478P0E10j"
my_api_secret          <- "NLrFfUgkfJVrs3UXoSSs4KGrzYFBu4BcjnpopIDM4az2EX3I4a"
my_access_token        <- "1252197116849672192-JkvDiU5dOe7WiLCn4BTXRPWQ3KpIoC"
my_access_token_secret <- "91oR1BrHclWQZZUygvZOeQZYb4Rml7zkvk4HtNnZtqYDU"
setup_twitter_oauth(my_api_key,my_api_secret,my_access_token,my_access_token_secret)

#scrape data emmawatson

tweets_emmawatson <- userTimeline("EmmaWatson", n=250)
tweets_df_emmawatson <- twListToDF(tweets_emmawatson)
dim(tweets_df_emmawatson)

head(tweets_df_emmawatson)

#extract the actual text and convert this corpus into a Term Document Matrix emmawatson

library(tm)

corpus <- Corpus(VectorSource(tweets_df_emmawatson$text))
tdm_emmawatson <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stopwords=TRUE, stemming=TRUE))
tdm_emmawatson

#visualize data emmawatson

library(wordcloud)

m1 <- as.matrix(tdm_emmawatson)
v1 <- sort(rowSums(m1), decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)
head(d1, 15)

wordcloud(words = d1$word, freq = d$freq, min.freq =2 ,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))




#scrape data borisjohnson

tweets_borisjohnson <- userTimeline("BorisJohnson", n=250)
tweets_df_borisjohnson <- twListToDF(tweets_borisjohnson)
dim(tweets_df_borisjohnson)
tweets_borisjohnson
head(tweets_df_borisjohnosn)

#extract the actual text and convert this corpus into a Term Document Matrix borisjohnson

library(tm)

corpus <- Corpus(VectorSource(tweets_df_borisjohnson$text))
tdm_borisjohnson <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stopwords=TRUE, stemming=TRUE))
tdm_borisjohnson



#scrape data theguardian

tweets_theguardian <- userTimeline("@guardian", n=250)
tweets_df_theguardian <- twListToDF(tweets_theguardian)
dim(tweets_df_theguardian)
tweets_theguardian
head(tweets_df_theguardian)

#extract the actual text and convert this corpus into a Term Document Matrix theguardian

library(tm)

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
