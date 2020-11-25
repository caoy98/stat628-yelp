######## Step1: Loading Data and Select Key Words ########

#a.read Chinese restaurants review file and load packages
library(tidytext); library(dplyr); library(wordcloud2); library(ggplot2); library(magrittr)
ch_review = read.csv("chinese_review.csv", stringsAsFactors = F)

#b.Targeted Words and Associated Adjective
meat = c("beef", "shrimp", "pork", "chicken", "crab", "duck", "lamb", "lobster") #Question: How to find the undesirable words?


#c.Tokenization and Getting Rid of The Stop Words
ch_filtered <- ch_review %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) > 3) #Notice: Don't forget to add the undesirable words
ch_filtered = select(ch_filtered, business_id, stars, category, word)

#d.Select Distinct Targeted Words
target <- function(x){
  ch_filtered %>% 
  filter(word == x) %>%
  select(business_id,stars,category,word) %>%
  distinct(business_id,.keep_all = TRUE)
}

for(i in meat){
  nam = paste(i, sep = " ")
  assign(nam, target(i))
}

######## Step 2: Created Data Frame for Word Frequency ########

#a.Get Rid of Words Appearing Less Than 10 Times on Reviews
ch_filtered = ch_filtered[ch_filtered$word %in% names(which(table(ch_filtered$word) > 10)), ]

#b.Find Associated Words
ch_adj = ch_filtered$word %>%
  filter(ch_filtered$word %in% c("Adjective")) %>%
  unique


######## Step 3: Analyzing Under Big Domain ######## 

#a. Barplot for Number of Times Each Meat Mentioned on Reviews
complete_mentioned = rbind(beef,crab,chicken,duck,frog,lamb,lobster,pork,shrimp)
mentioned_times = count(complete)
times_meat <- data.frame(count=c(556, 712, 499, 209, 24, 85, 135, 521, 575),
                         meat=c("beef","chicken","crab","duck","frog","lamb","lobster","pork","shrimp"))
ggplot(data=times_meat,aes(x=meat,y=count))+ geom_bar(stat="identity")

#b. Barplot for Restaurants with Different Stars
for (j in meat){
  nam = paste(j,"_stars",sep="")
  for (i in 1:5){
    a = j[j$stars == i,]
    b = nrow(a)
    c = rbind(c,b)
    assign(nam, c)
  }
}



