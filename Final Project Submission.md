Data Science Capstone - Final Project Submission
========================================================
author: Kalpana Das
date: August 8th, 2020
autosize: true
font-family: 'Helvetica'

Instructions
========================================================

The goal of this exercise is to create a product to highlight the prediction algorithm that you have built and to provide an interface that can be accessed by others. For this project you must submit:

- A Shiny app that takes as input a phrase (multiple words) in a text box input and outputs a prediction of the next word.
- A slide deck consisting of no more than 5 slides created with R Studio Presenter pitching your algorithm and app as if you were presenting to your boss or an investor.

Overview
========================================================

- A subset of the original data was sampled from the three sources (blogs, news and twitter) and then merged into one.
- We have understood the problem, collected and cleaned the data. Then we have done some exploratory analysis, statistical modeling, predictive modeling and a creative exploration of the data provided.
- The ultimate aim is to develop a Shiny app in R that can predict the next word using the previously entered text, similar to the one used in cellphone devices keyboard applications implemented by the SwiftKey.
- While using ShinyApp, the user should enter a phrase in the text box provided and shortly, the next predicted word would be obtained.


Algorithm 
========================================================

The steps followed to develop the predictive model using basic N-Grams model are:

- Downloaded the data, loaded the required packages, sampled the data, set the seed, computed the N-Grams, built models and then calculated the N-Grams log likelihood.
- Raw text sentences were split into words, all words were converted to lower case and punctuations, numbers, profane words, words not in the dictionary were filtered out.
- N-Gram probabilities were calculated and associated models were built to predict the next word, given an input phrase. The models were then compared and the best model was selected.
- Then the models were saved as R-compressed files (.RData files).

Links 
========================================================

- The link to my Shiny app is: https://kalpanadas.shinyapps.io/ShinyApp/
- The link to my GitHub repositotry (containing all the codes used) is: https://github.com/KalpanaDas/Data-Science-Capstone

