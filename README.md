# The Next Word App

This is the Capstone project Johns Hopkins University's Data Science Specialization in Coursera.

I developed The Next Word app ([link to the app](https://jussan.shinyapps.io/wordpredictionapp/)) which is based on [N-Gram Language Models](https://en.wikipedia.org/wiki/Word_n-gram_language_model). it's assumed that the probability of a word depends only on the previous word which is called a [Markov chain assumption](https://en.wikipedia.org/wiki/Markov_chain). Markov assumes the future state of a process only depends on its current state, not on the sequence of events that preceded it. Then the [Stupid Backoff algorithm](chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://aclanthology.org/D07-1090.pdf) is applied. Backoff means you go back to a n-1 gram level to calculate the probabilities when you encounter a word with probability equals 0. You can find the presentation deck for this App in this [link](https://rpubs.com/jussan/1077228).

- 'WordPrediction.R' contains all necessary functions for the App. 
- `DataPreparation_V2.R` contains the code that manipulates and prepare the three text files in which the app is based on. 
- `App.R` contains the code that creates the Shiny application. 

