rm(list=ls())

library(tidytext)
library(dplyr)
library(stringr)
library(plyr)
original_data <- read.csv("chinese_review.csv",header = TRUE, sep=",")
head(original_data)

review_id <- original_data$review_id
text <- original_data$text
stars <- original_data$stars

split_data <- original_data %>% 
  data.frame() %>%
  select(review_id,stars,text)

# For each review, create a word frequency vector via tokenization (R’s tidytext): p words
# Filter stopwords, stemming words and words with low frequency across reviews
tidy_data <- split_data %>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words)


freq <- count(tidy_data$word)

# Filter words with less than 10 frequency
words <- freq$x[freq$freq > 10]
# Filter words start with numeric
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
review_word_df <- as.data.frame(review_word_matrix)
colnames(review_word_df) <- words
head(review_word_df)

# Create an n-vector of star ratings
uniqueID <- c(1:n)
stars <- as.data.frame(cbind(uniqueID,stars))

# Distribution of specific words 
plotWordStar <- function(stars,DTM,wordList,mfrow = c(2,2)) {
  par(mfrow = mfrow)
  col_DTM = colnames(DTM)
  for(i in 1:length(wordList)) {
    index = which(col_DTM == wordList[i])
    if(length(index) == 0) {
      warning(paste(wordList[i],"not detected"))
      next
    } 
    dtm_vec = as.numeric(DTM[,index])
    names(dtm_vec) = rownames(DTM)
    starsY = rep(0,5)
    for(j in 1:5) {
      # I've changed this code to scale by total number of stars. This
      # I think provides better resolution than before.
      element = dtm_vec[as.character(stars$uniqueID[which(stars$stars == j)])]
      starsY[j]  = sum(element > 0,na.rm=TRUE) / sum(stars$stars == j)
    }
    barplot(starsY,main=wordList[i],xlab="Stars",ylab="Word Freq")
  }  
}

wordList <- c("service","tip","wifi","parking")
plotWordStar(stars = stars,DTM = review_word_df,wordList = wordList)
