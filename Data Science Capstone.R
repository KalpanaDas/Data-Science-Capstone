## This file contains the main R codes for this course.

# Downloading the data

if(!file.exists("./data")){dir.create("./data")
  Url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(Url, destfile="./data/Coursera-SwiftKey.zip", mode = "wb")
  unzip(zipfile="./data/Coursera-SwiftKey.zip", exdir="./data")}

# Loading the required packages 

library(NLP)
library(plyr)
library(magrittr)
library(stringr)
library(stringi)
library(tm)
library(SnowballC)
library(RWeka)
library(RColorBrewer)

# Sampling and reading the Data

number_of_lines <- 600000
data_Blogs <- readLines("./data/final/en_US/en_US.blogs.txt", number_of_lines)
data_News <- readLines("./data/final/en_US/en_US.news.txt", number_of_lines)
data_Twitter <- readLines("./data/final/en_US/en_US.twitter.txt", number_of_lines)

# Splitting 60% of data into training, 20% of data into validation and the remaning 20% of data into testing data sets
# Setting seed

set.seed(100)
idx <- sample(seq(number_of_lines)); N1 <- round(0.6* number_of_lines); N2 <- round(0.8* number_of_lines)
training <- c(data_Blogs[idx[1: N1]], data_News[idx[1:N1]], data_Twitter[idx[1: N1]])
validation <- c(data_Blogs[idx[(N1+1): N2]], data_News[idx[(N1+1):N2]], data_Twitter[idx[(N1+1): N2]])
testing <- c(data_Blogs[idx[(N2+1): number_of_lines]], data_News[idx[(N2+1): number_of_lines]], data_Twitter[idx[(N2+1): number_of_lines]])

# Further sampling from the training set

M <- 1000

# Computing UniGrams, BiGrams and TriGrams

UniGrams <- ngram_compute(training[1: M], n = 1, mat = 1) 
uni_m <- UniGrams %*% matrix(1, M, 1) 
names(uni_m) <- rownames(UniGrams)
uni_m <- sort(uni_m, decreasing = TRUE)

dictionary <- names(uni_m[1: 13500]) # Dictionary

BiGrams <- ngram_compute(training[1: M], n = 2, mat = 1, dictionary)
TriGrams <- ngram_compute(training[1: M], n = 3, mat = 1, dictionary)
bi_m <- BiGrams %*% matrix(1, ncol(BiGrams), 1); names(bi_m) <- rownames(BiGrams); bi_m <- sort(bi_m, decreasing = TRUE)
tri_m <- TriGrams %*% matrix(1, ncol(TriGrams), 1); names(tri_m) <- rownames(TriGrams); tri_m <- sort(tri_m, decreasing = TRUE)

# Building the model

M <- 10000
prune <- 2

uni.count <- ngram_compute(training[1: M], n = 1) # Unigram model
dictionary <- names(uni.count[uni.count>prune]) # Dictionary
bi.count <- ngram_compute(training[1: M], n = 2, dictionary = dictionary) # Bigram model
tri.count <- ngram_compute(training[1: M], n = 3, dictionary = dictionary) # Trigram model

# Calculating Unigram log likelihood

number_of_tokens <- sum(uni.count) # Total number of tokens
number_of_dictionary_words <- length(dictionary) # Number of words in the dictionary
uni.model <- rbind(as.matrix(uni.count[dictionary]), "UNK" = 0) + 1 # Laplacian smoothing
uni.model <- cbind(uni.model,log(uni.model[, 1]/ (number_of_tokens+number_of_dictionary_words)) ); colnames(uni.model) <- c("count", "MLE")

# Calculating Bigram log likelihood

bc.prune <- bi.count[bi.count > prune]
PQ <- names(bc.prune)
P <- sapply(strsplit(PQ, split = " "), function(x) x[1])
bi_m.1 <- as.matrix(bc.prune) + 1 # Laplacian smoothing
bi_m.1 <- cbind(bi_m.1, log(bi_m.1[, 1]/ uni.model[P, 1])) # "Word1" "Word2"
bi_m.2 <- as.matrix(cbind(rep(1, number_of_dictionary_words), log(1/ uni.model[1: number_of_dictionary_words, 1]))) # "Word1" "UNK"
rownames(bi_m.2) <- paste(dictionary, "UNK")
bi_m.3 <- as.matrix(cbind(1,uni.model["UNK",2])); rownames(bi_m.3) <- "UNK UNK" # "UNK" "UNK"
bi.model <- rbind(bi_m.1, bi_m.2, bi_m.3); colnames(bi.model) <- c("count", "MLE")

# Calculating trigram log likelihood

tc.prune <- tri.count[tri.count > prune]
PQR <- rownames(tc.prune)
PQ <- sapply(strsplit(PQR, split = " "), function(x) paste(x[1: 2], collapse = " "))
tri.model <- cbind(as.matrix(tc.prune+1), log(as.matrix(tc.prune+1)/ bi.model[PQ, 1]) )
colnames(tri.model) <- c("count", "MLE")

# Saving the models into R-compressed files

uni.mle <- sort(uni.model[, 2], decreasing = TRUE)
bi.mle <- sort(bi.model[, 2], decreasing = TRUE)
tri.mle <- sort(tri.model[, 2], decreasing = TRUE)

save(uni.mle, bi.mle, tri.mle, file = "data/Model.RData")

uni_perp <- ngram_perplexity(validation[1: 10000], uni.model, bi.model, tri.model, 1) # Unigram model perplexity
bi_perp <- ngram_perplexity(validation[1: 10000], uni.model, bi.model, tri.model, 2) # Bigram model perplexity
tri_perp <- ngram_perplexity(validation[1: 10000], uni.model, bi.model, tri.model, 3) # Trigram model perplexity


