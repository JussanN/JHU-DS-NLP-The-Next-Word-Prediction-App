library(LaF)
library(tidyverse)
library(tm)
library(textstem)
library(RWeka)
library(ggplot2)
library(dplyr)


#Create a Corpus and clean it
GetCorpus <- function(text){
  corpus <- VCorpus(VectorSource(text)) #transform text into a corpus
  corpus <- tm_map(corpus, content_transformer(tolower)) #lowering case
   # create custom function to remove other misc characters
  text_preprocessing <- function(x) {
    x <- gsub('http\\S+\\s*', '', x) # Remove URLs
    x <- gsub('#\\S+', '', x) # Remove hashtags
    x <- gsub('[[:cntrl:]]', '', x) # Remove controls and special characters
    x <- gsub("^[[:space:]]*", "", x) # Remove leading whitespaces
    x <- gsub("[[:space:]]*$", "", x) # Remove trailing whitespaces
    x <- gsub(" +", " ", x) # Remove extra whitespaces
    x <- gsub("[^[:alnum:][:space:]]", "", x) # Remove non-alphanumeric characters
    x <- gsub("\\b(\\w+)n't\\b", "\\1 not", x) # Expand contractions (e.g., "can't" -> "can not")
    x <- gsub("\\b(\\w+)'ve\\b", "\\1 have", x) # Expand "I've" -> "I have"
    x <- gsub("\\b(\\w+)'m\\b", "\\1 am", x) # Expand "I'm" -> "I am"
    x <- iconv(x, "latin1", "ASCII", sub = "") # Remove non-ASCII characters
    return(x)
  }
  # Now apply this function
  corpus <- tm_map(corpus, content_transformer(text_preprocessing))
  corpus <- tm_map(corpus, removePunctuation) #removes punctuation
  corpus <- tm_map(corpus, removeNumbers) #remove numbers
  #corpus <- tm_map(corpus, removeWords, stopwords("english")) #removing stopwords
  corpus <- tm_map(corpus, stripWhitespace) #removes whitespaces


  #corpus <- tm_map(corpus, content_transformer(stemDocument)) # Stemming

  #corpus<- tm_map(corpus, content_transformer(lemmatize_strings)) # Lemmatization
  corpus <- tm_map(corpus, PlainTextDocument)

  return(corpus)
}

GetToken <- function(corpus, ngram){
  #Generate Document Term matrix for NGrams
  corpus <- corpus
  ngram <- ngram
  dtm = DocumentTermMatrix(corpus, control = list(tokenize = function(x) {
    NGramTokenizer(x, Weka_control(min = ngram, max = ngram))
  }))

  return(dtm)

}


#Get the frequency of the words
GetFreq <- function(dtm){
  matrix_dtm <- as.matrix(dtm)
  bigrams_frequency <- sort(colSums(matrix_dtm),decreasing=TRUE)

  df_freq <- data.frame(word=names(bigrams_frequency),
                        freq=bigrams_frequency)

  return(df_freq)
}

PlotFreq <- function(df, frequency_threshold){
  #Create a frequency plot
  frequency_plot <- ggplot(subset(df, freq>frequency_threshold),
                           aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity", fill = "#FF6666") +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  frequency_plot
}

#Backoff prediction
BackOffPrediction <- function(input, df_unigram, df_bigram, df_trigram, discount_factor = 1) {

  # Lowercase the input
  input <- tolower(input)
  inputByWord <- strsplit(input, ' ')[[1]]

  # Character count in the input
  charCount <- length(inputByWord)

  # Default fallback: most frequent unigram
  most_frequent_word <- df_unigram$first_word[which.max(df_unigram$freq)]

  # Helper function to rank predictions
  rank_predictions <- function(df, word_col) {
    df %>%
      arrange(desc(freq)) %>%
      mutate(Rank = row_number()) %>%
      select(Next_word = !!sym(word_col), Rank)
  }

  if (charCount >= 2) {
    # Check trigram first
    df_trigram_match <- df_trigram %>%
      filter(
        first_word == inputByWord[charCount - 1],
        second_word == inputByWord[charCount]
      )

    if (nrow(df_trigram_match) > 0) {
      # Trigram prediction
      df_rank <- rank_predictions(df_trigram_match, "third_word")
    } else {
      # Fall back to bigram
      df_bigram_match <- df_bigram %>%
        filter(first_word == inputByWord[charCount])

      if (nrow(df_bigram_match) > 0) {
        df_rank <- rank_predictions(df_bigram_match, "second_word")
      } else {
        # Fall back to unigram
        df_rank <- rank_predictions(df_unigram, "first_word")
      }
    }
  } else if (charCount == 1) {
    # Bigram prediction for single word input
    df_bigram_match <- df_bigram %>%
      filter(first_word == inputByWord[charCount])

    if (nrow(df_bigram_match) > 0) {
      df_rank <- rank_predictions(df_bigram_match, "second_word")
    } else {
      # Fall back to unigram
      df_rank <- rank_predictions(df_unigram, "first_word")
    }
  } else {
    # Empty input case
    stop("Please enter a sentence")
  }

  # Add fallback if no match found in all n-grams
  if (nrow(df_rank) == 0) {
    df_rank <- data.frame(Next_word = most_frequent_word, Rank = 1)
  }

  # Rename columns
  colnames(df_rank) <- c("Next word", "Rank")

  # Return only top 3 predictions
  return(head(df_rank, 3))
}
