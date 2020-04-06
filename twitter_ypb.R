#ONE:
#1) Extract tweets for any user (try choosing a user who has more tweets)
#2) Perform sentimental analysis on the tweets extracted from the above

library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

cred <- OAuthFactory$new(consumerKey='H9zKemMpxFGrtbyIekvpp06o6', # Consumer Key (API Key)
                         consumerSecret='jtTyGFOmabjVTVEMw6me0deuUJn1owozatDMQQB54pJ6tVUyDg', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Access Token Secret
setup_twitter_oauth("H9zKemMpxFGrtbyIekvpp06o6", # Consumer Key (API Key)
                    "jtTyGFOmabjVTVEMw6me0deuUJn1owozatDMQQB54pJ6tVUyDg", #Consumer Secret (API Secret)
                    "2186123106-tLdRHgnFC5yRjG35ywp1TpYwmephwmrzEFXjjLo",  # Access Token
                    "YgyEFvdzFgnMZHUhoq3rcaNvsF1h0HyCbHYZdc9NMgUia")  #Access Token Secret


Tweets <- userTimeline('SGanguly99', n = 2000, includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

setwd("D:/YOGESH/EXCELR DATA SCIENCE/DATA SCI_NITIN SIR/Text mining")
write.csv(TweetsDF, "Tweets.csv",row.names = F)

getwd()

# Read file
twitter <- read.csv(file.choose())
str(twitter)

# Build Corpus and DTM/TDM
corpus <- twitter$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus_clean[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

# Since the word bcci, kolkata  is used more, this can be removed as we are 
# mining the tweets from Twitter only.

cleanset<-tm_map(cleanset,removeWords, c('bcci','kolkata'))

cleanset <- tm_map(cleanset, gsub,pattern = 'pages', replacement = 'page')

# the barplot pulls both page and pages as separate words. this should be 
# counted as one.

inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

# the terms indicate that there are 2512 words and 762 documents(# of tweets) in this TDM
# Sparsity is 100% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))

# Word Cloud :

w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 400,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 1, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

# lettercloud 

letterCloud(w,word = "F",frequency(5), size=1)


# Sentiment Analysis for tweets:


# install.packages("syuzhet")

# Read File 
tweetdata <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(tweetdata$text)
class(tweets)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(tweets)
head(s)

tweets[4]
get_nrc_sentiment('pretending')

# Pretend has one value of negative and one value for anger
get_nrc_sentiment('can learn') #1 for positive

# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Sourav Ganguly Tweets')

