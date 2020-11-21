# Goal: Explore businesses that serve Chinese food on Yelp and gather insights about the Chinese food businesses through Yelp reviews.
# Data cleaning
rm(list=ls())
# (Filter businesses(and associated reviews) with key words "Chinese" from Yelp fusion API)
original_data <- read.csv("chinese_review.csv",header = TRUE, sep=",")
head(original_data)
n <- nrow(original_data)
m <- length(unique(original_data$business_id))

# For each review, create a word frequency vector via tokenization (R’s tidytext): p words
library(dplyr)
library(tidytext)

original_df <- data.frame(original_data)
tidy_data <- original_df %>% unnest_tokens(word,text,drop = FALSE)

# Filter stopwords, stemming words and words with low frequency across reviews
my_stopwords <- tibble(word = c(as.character(1:100)))
word_freq <- tidy_data %>% count(word,sort = TRUE)
lowfre_words <- tibble(word = word_freq$word[which(word_freq$n<10)])
tidy_data <- tidy_data %>% anti_join(stop_words) %>% anti_join(my_stopwords) %>% anti_join(lowfre_words)

