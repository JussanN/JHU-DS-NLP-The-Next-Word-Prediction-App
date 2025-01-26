#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("WordPrediction.R")

load("dataset_unigram.Rda")
load("dataset_bigram.Rda")
load("dataset_trigram.Rda")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The Next Word app"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          textInput("sentence", label = NULL, value = "", width = NULL, placeholder = NULL),
          #submitButton("Submit", icon("refresh")),
          br(),
          h3("What is the The Next Word app:"),
          p("This app predicts the next word based on the text entered by the user"),
          h4("The step is:"),
          p("Enter a sentence in the text field without punctuation, symbols or numbers and type space after the last word"),
          p("The score of each predicted word can be visualized in the main panel.")

          ),

        # Show a a table
        mainPanel(
           tableOutput("table"),
           h3("How to read the table above:"),
           p("This is table shows the top possible next words and it's rank. Higher in the rank, higher is the probability that word is the next one"),
           h3("How does the prediction model work:"),
           "The Next Word app is based on",
           tags$a(href="https://en.wikipedia.org/wiki/Word_n-gram_language_model",
                  "N-Gram Language Models."),
           "it's assumed that the probability of a word depends only on the previous word which is called a",
           tags$a(href="https://en.wikipedia.org/wiki/Markov_chain",
                  "Markov chain assumption."),
           "Markov assumes the future state of a process only depends on its current state, not on the sequence of events that preceded it.",
           "Then the",
           tags$a(href="https://aclanthology.org/D07-1090.pdf",
                  "Stupid Backoff"),
           "algorithm is applied. Backoff means you go back to a n-1 gram level to calculate the probabilities when you encounter a word with probability equals 0:",
           p("1. Using 3-grams to calculate the probability of a word in text. You have \"I am\" followed by \"tired\". If \"tired\" never occurred in the sequence \"I am tired\" therefore the 3-grams model \"tired\" has probability 0."),
           p("2. Then we go back to a 2-gram \"am\" followed by \"tired\" and check if the probability is different from 0. If yes,"),
           p("3. We go back to a unigram model.")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- renderTable({
      req(input$sentence)
      df <- BackOffPrediction(tolower(input$sentence), df_freq_unigram, df_freq_bigram, df_freq_trigram)
      validate(
        need(nrow(df) > 0, "Please enter a sentence in the text field")
      )
      # Since BackOffPrediction already returns the top 3, no need to filter again
      df  # Return the predictions (top 3) directly

    }, width = "200px")

}

# Run the application
shinyApp(ui = ui, server = server)
