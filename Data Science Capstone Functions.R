### This file contains the R codes of the functions used for this course.

## Tokenizing the text into words

ngram_tokenize <- function(input_data){
  for(i in 1: length(input_data)){
    xyz <- input_data[i]
    xyz <- gsub("[.-]", " ", xyz) # Replacing . - with space
    xyz <- gsub("[[:punct:]]", "", xyz) # Removing punctuations
    xyz <- gsub("[0-9]", "", xyz) # Removing numbers
    input_data[i] <- tolower(xyz) # Converting to lower case alphabets
  }
  input_data <- lapply(input_data, function(x) unlist(strsplit(x, split = " ")) ) # Converting into words
  input_data <- lapply(input_data, function(x) grep("^[a-z]+$", x, value = TRUE) ) # Using only English words
  
  # Removing profane words 
  
  profane_words <- readLines("./data/profanities.txt")
  remove_words <- c(profane_words[-1]) 
  input_data <- lapply(input_data, function(x) { x[!(x %in% remove_words)] })
}


## Computing the counts of N-Grams (analogous to NGramTokenizer() + TermDocumentMatrix())

ngram_compute <- function(input_data, n, mat = 0, dictionary = character()){
  input_data <- ngram_tokenize(input_data) # Tokenizing the data
  
  # Creating N-Grams
  
  if(n > 1) { input_data <- sapply(input_data, function(x) x[x %in% dictionary]) } # Select words in the dictionary
  if(n == 2) { # Bigrams
    idx2 <- sapply(input_data, function(x) ifelse(length(x) > 1, TRUE, FALSE)) 
    input_data <- lapply(input_data[idx2], function(x) { paste(x[1: (length(x) - 1)], x[2: length(x)]) })
  }
  if(n == 3) { # Trigrams
    idx3 <- sapply(input_data, function(x) ifelse(length(x) > 2, TRUE, FALSE)) 
    input_data <- lapply(input_data[idx3], function(x) { paste(x[1: (length(x) - 2)], x[2: (length(x) - 1)], x[3: length(x)]) })
  }
  
  # Counting unique N-Grams
  
  if(mat == 1) { # Simulating the Term Document Matrix
    length_input_data <- length(input_data)
    unique_ngrams <- unique(unlist(input_data))
    ngram <- matrix(0, length(unique_ngrams), length_input_data); rownames(ngram) <- unique_ngrams; colnames(ngram) <- 1: length_input_data
    for(i in 1: length_input_data) { ns <- table(input_data[[i]]);  ngram[names(ns), i] <- ns }
  }
  else { # Getting N-Gram counts
    ngram <- sort(table(unlist(input_data)), decreasing = TRUE)
  }
  ngram
}


## Computing Bigram MLE of a test data set and against a dictionary to predict the next word

bigram_mle <- function(input_data, uni.model, bi.mle, flag = 0){
  input_data[!(input_data %in% rownames(uni.model))] <- "UNK"; 
  length_input_data <- length(input_data)
  if(flag == 0){ 
    abc <- input_data[1: (length_input_data - 1)]
    def <- input_data[2: length_input_data]
  } 
  else { 
    def <- rownames(uni.model); def <- def[1: (length(def) - 1)] # Against dictionary  
    abc <- rep(input_data[length_input_data], length(def))
  }
  abcdef <- paste(abc, def)
  id1 <- (abcdef %in% rownames(bi.model))
  id2 <- (!id1) & (abc != "UNK")
  id3 <- (!id1) & (abc == "UNK")
  a <- matrix(NA, length(abcdef), 1)
  if(sum(id1)>0) { a[id1] <- bi.model[abcdef[id1], 2] }
  if(sum(id2)>0) { a[id2] <- log(1/uni.model[abc[id2], 1]) }
  if(sum(id3)>0) { a[id3] <- uni.model["UNK", 2] }
  rownames(a) <- abcdef; colnames(a) <- "MLE"
  a
}


## Computing Trigram MLE of a test data set and against a dictionary to predict the next word

trigram_mle <- function(input_data, uni.model, bi.model, tri.model, flag = 0){
  input_data[!(input_data %in% rownames(uni.model))] <- "UNK"; length_input_data <- length(input_data)
  if(flag == 0){
    P <- input_data[1: (length_input_data - 2)]; Q <- input_data[2: (length_input_data - 1)]; R <- input_data[3: length_input_data]
  }
  else { 
    R <- rownames(uni.model); R <- R[1: (length(R) - 1)] # Against dictionary
    P <- rep(input_data[length_input_data - 1],length(R)) 
    Q <- rep(input_data[length_input_data], length(R))
  }
  PQR <- paste(P, Q, R)
  id1 <- (PQR %in% rownames(tri.model))
  PQ <- paste(P, Q)
  id2 <- (!id1) & (PQ %in% rownames(bi.model))
  id3 <- (!id1) & (!(PQ %in% rownames(bi.model))) & (P != "UNK")
  id4 <- (!id1) & (!(PQ %in% rownames(bi.model))) & (P == "UNK")
  
  a <- matrix(NA, length(PQR), 1)
  if(sum(id1)>0) { a[id1] <- tri.model[PQR[id1], 2] }
  if(sum(id2)>0) { a[id2] <- log(1/bi.model[PQ[id2], 1]) }
  if(sum(id3)>0) { a[id3] <- log(1/uni.model[P[id3], 1]) }
  if(sum(id4)>0) { a[id4] <- uni.model["UNK", 2] }
  
  rownames(a) <- PQR; colnames(a) <- "MLE"
  a
}


## Computing the perplexity of a test data set

ngram_perplexity <- function(input_data, uni.model, bi.model, tri.model, n){
  D <- ngram_tokenize(input_data); D <- unlist(D); 
  if(n == 1) { # Unigram 
    D[!(D %in% rownames(uni.model))] <- "UNK"
    a <- uni.model[D, 2]
    n_perplexity <- exp(-sum(a)/ length(a))
  }
  if(n == 2) { # Bigram 
    a <- bigram_mle(D, uni.model, bi.model)
    n_perplexity <- exp(-sum(a)/ length(a))
  }
  if(n == 3) { # Trigram 
    a <- trigram_mle(D, uni.model, bi.model, tri.model)
    n_perplexity <- exp(-sum(a)/ length(a))
  }
  n_perplexity
}


## Predicting the next word given a sequence of words

ngram_predict <- function(input_data){
  
  D <- ngram_tokenize(input_data) # Tokenizing
  D <- unlist(D); D[!(D %in% names(uni.mle))] <- "UNK" # Labelling the ones not in dictionary as UNK
  L <- length(D)
  
  if(L == 0) { P <- "UNK"; Q <- "UNK"}
  if(L == 1) { P <- "UNK"; Q <- D[L] }
  if(L>1) { P <- D[L - 1]; Q <- D[L] }
  R <- names(uni.mle); R <- R[-length(R)] # Against dictionary
  
  PQR <- paste(P, Q, R)
  id1 <- (PQR %in% names(tri.mle))
  PQ <- paste(P, Q)
  QR <- paste(Q, R)
  id2 <- (!id1) & (PQ %in% names(bi.mle)) & (QR %in% names(bi.mle))
  id3 <- (!id1) & (PQ %in% names(bi.mle)) & (!(QR %in% names(bi.mle)))
  id4 <- (!id1) & (!(PQ %in% names(bi.mle))) & (QR %in% names(bi.mle))
  id5 <- (!id1) & (!(PQ %in% names(bi.mle))) & (!(QR %in% names(bi.mle)))
  
  a <- matrix(NA, length(PQR), 1)
  if(sum(id1)>0) { a[id1] <- tri.mle[PQR[id1]] }
  if(sum(id2)>0) { a[id2] <- bi.mle[PQ] + bi.mle[QR[id2]] + log(2*0.4) }
  if(sum(id3)>0) { a[id3] <- bi.mle[PQ] + uni.mle[Q] + uni.mle[R[id3]] + log(3*0.4) }
  if(sum(id4)>0) { a[id4] <- uni.mle[P] + uni.mle[Q] + bi.mle[QR[id4]] + log(3*0.4) }
  if(sum(id5)>0) { a[id5] <- uni.mle[P] + uni.mle[Q] + uni.mle[R[id5]] + log(4*0.4) }
  
  predict_word <- R[which.max(a)]
}