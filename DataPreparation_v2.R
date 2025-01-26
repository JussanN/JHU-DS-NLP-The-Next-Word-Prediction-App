library(LaF)
library(tidyverse)
library(tm)
library(textstem)
library(RWeka)
library(ggplot2)
library(dplyr)
source("WordPredictionApp/WordPrediction.R")

# Read in files
twitter <- "en_US.twitter.txt"
blogs <- "en_US.blogs.txt"
news <- "en_US.news.txt"

# Sample lines (20000 lines for now, can adjust if needed)
set.seed(1)
sample_twitter <- sample_lines(twitter, 54000)
sample_twitter <- str_replace_all(sample_twitter, pattern= "[&â€¦™ðŸ¥]", replacement = "")
sample_blogs <- sample_lines(blogs, 54000)
sample_blogs <- str_replace_all(sample_blogs, pattern= "[&â€¦™ðŸ¥]", replacement = "")
sample_news <- sample_lines(news, 39000)
sample_news <- str_replace_all(sample_news, pattern= "[&â€¦™ðŸ¥]", replacement = "")

# Combine text from files into one sample
combined_sample <- c(sample_twitter, sample_blogs, sample_news)

# Get corpus from combined sample
corpus <- GetCorpus(combined_sample)

# Function to process text in chunks of 3000 lines
process_in_chunks <- function(corpus, ngram_size, chunk_size = 3000) {
  # Calculate the number of chunks
  chunk_count <- ceiling(length(corpus) / chunk_size)

  # Initialize empty dataframe for term frequencies
  term_frequencies <- data.frame(word = character(), freq = integer())

  for (i in 1:chunk_count) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, length(corpus))
    corpus_chunk <- corpus[start_idx:end_idx]

    # Create DTM for the chunk (no sparse term removal)
    dtm_chunk <- GetToken(corpus_chunk, ngram_size)

    # Get frequencies for the chunk
    chunk_freq <- GetFreq(dtm_chunk)

    # Combine chunk frequencies with existing ones
    term_frequencies <- bind_rows(term_frequencies, chunk_freq)
  }

  # Aggregate frequencies across all chunks
  term_frequencies <- term_frequencies %>%
    group_by(word) %>%
    summarise(freq = sum(freq)) %>%
    arrange(desc(freq))

  return(term_frequencies)
}

# Process unigrams, bigrams, and trigrams in chunks (chunk_size = 3000)
df_freq_unigram_chunk <- process_in_chunks(corpus, 1)
df_freq_bigram_chunk <- process_in_chunks(corpus, 2)
df_freq_trigram_chunk <- process_in_chunks(corpus, 3)

# Plot frequencies
PlotFreq(df_freq_unigram_chunk, df_freq_unigram_chunk$freq[10])
PlotFreq(df_freq_bigram_chunk, df_freq_bigram_chunk$freq[10])
PlotFreq(df_freq_trigram_chunk, df_freq_trigram_chunk$freq[10])

# Clean up and separate terms for bigram and trigram
df_freq_unigram <- df_freq_unigram_chunk %>%
  rename(first_word = word)

df_freq_bigram <- df_freq_bigram_chunk %>%
  separate(word, c('first_word', 'second_word'))

df_freq_trigram <- df_freq_trigram_chunk %>%
  separate(word, c('first_word', 'second_word', 'third_word'))

# Save results
save(df_freq_unigram, file="WordPredictionApp/dataset_unigram.Rda")
save(df_freq_bigram, file="WordPredictionApp/dataset_bigram.Rda")
save(df_freq_trigram, file="WordPredictionApp/dataset_trigram.Rda")


