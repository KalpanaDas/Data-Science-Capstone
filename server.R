# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

library(shiny)
load("Model.RData")

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
  
  profane_words <- readLines("./profanities.txt")
  remove_words <- c(profane_words[-1]) 
  input_data <- lapply(input_data, function(x) { x[!(x %in% remove_words)] })
}

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

shinyServer(function(input, output){
  output$prediction <- renderPrint({
    result <- ngram_predict(input$InputString)
    result});
  output$text_in <- renderText({input$InputString});
  
})
