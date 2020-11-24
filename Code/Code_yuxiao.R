rm(list=ls())

library(tidytext)
library(dplyr)
library(stringr)
library(plyr)
original_data <- read.csv("chinese_review.csv",header = TRUE, sep=",")
head(original_data)

text <- original_data$text
review_id <- original_data$review_id
split_data <- original_data %>% 
  data.frame() %>%
  select(review_id,text)

# For each review, create a word frequency vector via tokenization (R’s tidytext): p words
# Filter stopwords, stemming words and words with low frequency across reviews
tidy_data <- split_data %>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

freq <- count(tidy_data$word)

# Filter words with less than 10 frequency
words <- freq$x[freq$freq > 10]
words <- words[!str_sub(words,1,1) %in% c(0:9)]
tidy_data <- tidy_data %>% 
  subset(tidy_data$word %in% words)

# Create a review-to-word n x p matrix with n reviews, p words
p <- length(words)
n <- length(text)

review_word_list <- list()

for (i in 1:n){
  index <- which(words %in% tidy_data$word[(tidy_data$review_id==review_id[i])])
  v <- rep(0,p)
  v[index] <- count(tidy_data$word[(tidy_data$review_id==review_id[i])])$freq
  review_word_list[[i]] <- v
}

review_word_matrix <- matrix(unlist(review_word_list),nrow = n)
colnames(review_word_matrix) <- words

# Distribution of specific words 
