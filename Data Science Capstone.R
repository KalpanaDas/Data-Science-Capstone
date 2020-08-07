# This file contains the main R codes for this course.

## Downloading the data
if(!file.exists("./data")){dir.create("./data")
  Url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(Url, destfile="./data/Coursera-SwiftKey.zip", mode = "wb")
  unzip(zipfile="./data/Coursera-SwiftKey.zip", exdir="./data")}

## Loading the required packages 
library(plyr)
library(magrittr)
library(stringr)
library(stringi)
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(RWeka)

## Reading the Data
data_Blogs <- readLines("./data/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
data_News <- readLines("./data/final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
data_Twitter <- readLines("./data/final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

## Summary statistics of the Datasets
stri_stats_general(data_Blogs)
stri_stats_general(data_News)
stri_stats_general(data_Twitter)

## Sampling the data and making the corpus
subdata_Blogs <- sample(data_Blogs, size = 1000)
subdata_News <- sample(data_News, size = 1000)
subdata_Twitter <- sample(data_Twitter, size = 1000)
sampled_Data <- c(subdata_Blogs, subdata_News, subdata_Twitter)
corpus <- VCorpus(VectorSource(sampled_Data))

## Removing numbers, punctuations, stopwords, white spaces, etc. from the corpus
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "/|@|//|$|:|:)|*|&|!|?|_|-|#|")
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

## Creating the Term-Document-Matrices
dtm_1 <- TermDocumentMatrix(corpus)
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm_2 <- TermDocumentMatrix(corpus, control = list(tokenize = bigram))
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm_3 <- TermDocumentMatrix(corpus, control = list(tokenize = trigram))

## Making N-Grams
tdm_Ngram <- function (text, n) {
  NgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))}
  tdm_ngram <- TermDocumentMatrix(text, control = list(tokenizer = NgramTokenizer))
  tdm_ngram
  }

## Extracting the N-Grams and sorting
ngram_sorted_df <- function (tdm_ngram) {
  tdm_ngram_m <- as.matrix(tdm_ngram)
  tdm_ngram_df <- as.data.frame(tdm_ngram_m)
  colnames(tdm_ngram_df) <- "Count"
  tdm_ngram_df <- tdm_ngram_df[order(-tdm_ngram_df$Count), , drop = FALSE]
  tdm_ngram_df
  }

# Calculating the N-Grams
tdm_1gram <- tdm_Ngram(corpus, 1)
tdm_2gram <- tdm_Ngram(corpus, 2)
tdm_3gram <- tdm_Ngram(corpus, 3)
tdm_4gram <- tdm_Ngram(corpus, 4)

# Extracting the term count tables from N-Grams and sorting 
tdm_1gram_df <- ngram_sorted_df(tdm_1gram)
tdm_2gram_df <- ngram_sorted_df(tdm_2gram)
tdm_3gram_df <- ngram_sorted_df(tdm_3gram)
tdm_4gram_df <- ngram_sorted_df(tdm_4gram)

# Saving the data frames into R-compressed files

quadgram <- data.frame(rows = rownames(tdm_4gram_df),count = tdm_4gram_df$Count)
quadgram$rows <- as.character(quadgram$rows)
quadgram_split <- strsplit(as.character(quadgram$rows),split = " ")
quadgram <- transform(quadgram,first = sapply(quadgram_split,"[[",1),second = sapply(quadgram_split,"[[",2),third = sapply(quadgram_split,"[[",3), fourth = sapply(quadgram_split,"[[",4))
quadgram <- data.frame(unigram = quadgram$first,bigram = quadgram$second, trigram = quadgram$third, quadgram = quadgram$fourth, freq = quadgram$count,stringsAsFactors=FALSE)
write.csv(quadgram[quadgram$freq > 1,],"./ShinyApp/quadgram.csv",row.names=F)
quadgram <- read.csv("./ShinyApp/quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"./ShinyApp/quadgram.RData")

trigram <- data.frame(rows = rownames(tdm_3gram_df),count = tdm_3gram_df$Count)
trigram$rows <- as.character(trigram$rows)
trigram_split <- strsplit(as.character(trigram$rows),split = " ")
trigram <- transform(trigram,first = sapply(trigram_split,"[[",1),second = sapply(trigram_split,"[[",2),third = sapply(trigram_split,"[[",3))
trigram <- data.frame(unigram = trigram$first,bigram = trigram$second, trigram = trigram$third, freq = trigram$count,stringsAsFactors=FALSE)
write.csv(trigram[trigram$freq > 1,],"./ShinyApp/trigram.csv",row.names=F)
trigram <- read.csv("./ShinyApp/trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"./ShinyApp/trigram.RData")

bigram <- data.frame(rows = rownames(tdm_2gram_df),count = tdm_2gram_df$Count)
bigram$rows <- as.character(bigram$rows)
bigram_split <- strsplit(as.character(bigram$rows),split = " ")
bigram <- transform(bigram,first = sapply(bigram_split,"[[",1),second = sapply(bigram_split,"[[",2))
bigram <- data.frame(unigram = bigram$first,bigram = bigram$second,freq = bigram$count,stringsAsFactors=FALSE)
write.csv(bigram[bigram$freq > 1,],"./ShinyApp/bigram.csv",row.names=F)
bigram <- read.csv("./ShinyApp/bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"./ShinyApp/bigram.RData")

## Word Cloud visual representation
wordcloud(corpus, max.words = 100, random.order = FALSE, rot.per=0.30, use.r.layout = TRUE, colors = brewer.pal(10, "Dark2"))

## 1-Gram Frequency plot
freq1 <- rowSums(as.matrix(dtm_1))
freq1 <- sort(freq1, decreasing = TRUE)
dfFreq1 <- data.frame(word = names(freq1), freq = freq1)
ggplot(dfFreq1[1:20, ], aes(word, freq)) +
  geom_bar(stat = "identity", colour = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("1-Gram Frequency")

## 2-Gram Frequency plot
freq2 <- rowSums(as.matrix(dtm_2))
freq2 <- sort(freq2, decreasing = TRUE)
dfFreq2 <- data.frame(word = names(freq2), freq = freq2)
ggplot(dfFreq2[1:20, ], aes(word, freq)) +
  geom_bar(stat = "identity", colour = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("2-Gram Frequency")

## 3-Gram Frequency plot
freq3 <- rowSums(as.matrix(dtm_3))
freq3 <- sort(freq3, decreasing = TRUE)
dfFreq3 <- data.frame(word = names(freq3), freq = freq3)
ggplot(dfFreq3[1:20, ], aes(word, freq)) +
  geom_bar(stat = "identity", colour = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("3-Gram Frequency")
