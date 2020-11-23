## read Chinese restaurants review file and load packages

library(tidytext); library(dplyr); library(wordcloud2); library(ggplot2); library(magrittr)
ch_review = read.csv("chinese_review.csv", stringsAsFactors = F)

## Targeted Words and Associated Adjective
meat = c("beef", "shrimp", "pork", "chicken", "crab", "duck", "lamb", "lobster")
#Question: How to find the undesirable words?


## Tokenization and Getting Rid of The Stop Words
Ch_filtered <- ch_review %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) > 3)
#Notice: Don't forget to add the undesirable words


## Select Distinct Targeted Words
target <- function(x){
    Ch_filtered %>% 
    filter(word == x) %>%
    select(business_id,stars,category,word) %>%
    arrange() %>%
    distinct(business_id,.keep_all = TRUE)
}
for(i in meat){
  nam = paste(i, sep = " ")
  assign(nam, target(i))
}

## Category Plot
complete_mentioned = rbind(beef,crab,chicken,duck,frog,lamb,lobster,pork,shrimp)
mentioned_times = count(complete)
times_meat <- data.frame(count=c(556, 712, 499, 209, 24, 85, 135, 521, 575),
                         meat=c("beef","chicken","crab","duck","frog","lamb","lobster","pork","shrimp"))
ggplot(data=times_meat,aes(x=meat,y=count))+ geom_bar(stat="identity")





