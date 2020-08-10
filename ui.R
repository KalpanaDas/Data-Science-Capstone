# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

library(shiny)
shinyUI(navbarPage("Data Science Capstone Project",
                   tabPanel("Predicting the next word",
                            # Sidebar
                            sidebarLayout(
                              sidebarPanel(
                                h5('Author: Kalpana Das'),
                                helpText("Input a phrase (multiple words) in English to 
                                get an output, i.e. a prediction of the next word. For 
                                         example, 'two years'."),
                                textInput("InputString", "Enter a phrase", value = "")
                               ),
                              
                              mainPanel(
                                h2("Predicting the next word"),
                                h5("Kindly wait for some time for the application to run 
                                   and the results to load."),
                                verbatimTextOutput("prediction"),
                                h4("Input phrase: "),
                                verbatimTextOutput('text_in'),
                              ))),
                   
                   tabPanel("Background",
                            h5('The aim of this application is to predict the next word using 
                            the entered input text, similar to the one used in cellphone devices 
                            keyboard applications implemented by " Swift-Key ". This works based on 
                               the " Stupid Back Off " and " N-Gram Tokenizer " algorithm. The following were considered 
                               in Unigram, Bigram and Trigram model building:'),
                            h5('1. Smoothing: Laplace smoothing of adding 1 to all the counts was done to
                               calculate probabilities for missing unigrams.'),
                            h5('2. Pruning: N-Grams with less than two counts were not taken into account 
                               into the model directly instead they were indirectly accounted for by smoothing.'),
                            h5('N-Gram based language models were built on the sampled training set and are used 
                            to predict the next word from the previous (N-1) words of the phrase. All three models 
                            were considered with Stupid Back Off to roll back to the next best model if an N-Gram 
                            did not exist in the language model. To predict the next word first Trigram is used. 
                            The first two words of Trigram are the last two words of the phrase input. If no Trigram 
                            is found back off to Bigram. The first word of Bigram is the last word of the phrase 
                            input. If no Bigram is found back off to "the" (the most frequent unigram). More about 
                            this application and project is available at  my GitHub repositotry. The link to it is below.'),

                            tags$a(href = "https://github.com/KalpanaDas/Data-Science-Capstone", 
                                   "https://github.com/KalpanaDas/Data-Science-Capstone")
                            
                            
                   )
)
)

