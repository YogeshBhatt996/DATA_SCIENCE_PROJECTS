#TWO:
#1) Extract reviews of any product from ecommerce  amazon
#2) Perform sentimental analysis

library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/product-reviews/0751565369/ref=acr_dpproductdetail_text?ie=UTF8&showViewpoints=1"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)

setwd("D:/YOGESH/EXCELR DATA SCIENCE/DATA SCI_NITIN SIR/Text mining")
write.table(amazon_reviews,"MSIGTGN.txt",row.names = F)


MSIGTGN_Lap <- read.delim('MSIGTGN.TXT')
str(MSIGTGN_Lap)

View(MSIGTGN_Lap)

# Build Corpus and DTM/TDM
library(tm)
corpus <- MSIGTGN_Lap[-1,]
head(corpus)

class(corpus)

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
inspect(corpus[1:5])

cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords, c('harry','potter'))

# Since the word harry and potter is used more, this can be removed as we are 
# mining the tweets for this miphone only.


cleanset <- tm_map(cleanset, gsub,pattern = 'harry', replacement = 'potter')
# the barplot pulls both harry and potter as separate words. this should be 
# counted as one as both holds the same synonym.

inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix :
# Convert the unstructured data to structured data :
tdm <- TermDocumentMatrix(cleanset)
tdm

# the terms indicate that there are 342 words and 99 documents(# of tweets) in this TDM
# Sparsity is 88% which indicates that there are lots of zero values.
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 

w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))


# Word Cloud :
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)

# lettercloud 

letterCloud(w,word = 'Am',frequency(5), size=1)

# Sentiment Analysis for tweets:
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# install.packages("syuzhet")

# Read File 
Amzn_reviews <- read.delim('MSIGTGN.TXT')
reviews <- as.character(Amzn_reviews[-1,])
class(reviews)

# Obtain Sentiment scores 
s <- get_nrc_sentiment(reviews)
head(s)

reviews[4]

get_nrc_sentiment('Love')#1 joy and 1 positive

# Love has one Joy and one positive 
get_nrc_sentiment('difficult') #1 Anger and 1 Negavite




# barplot 

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Harry Potter and the Cursed Child')

