######## Step1: Loading Data and Select Key Words ########

#a.read Chinese restaurants review file and load packages
library(tidytext); library(dplyr); library(wordcloud2); library(ggplot2); library(magrittr); library(rlist);library(gridExtra)
ch_review = read.csv("chinese_review.csv", stringsAsFactors = F)

#b.Targeted Words and Associated Adjective
meat = c("beef", "shrimp", "pork", "chicken","frog", "crab", "duck", "lamb", "lobster") #Question: How to find the undesirable words?


#c.Tokenization and Getting Rid of The Stop Words
ch_filtered <- ch_review %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) > 3) #Notice: Don't forget to add the undesirable words
ch_filtered = select(ch_filtered, business_id, stars, category, word)

#d.Select Targeted Words
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

com_meats = rbind(beef,chicken,crab,duck,lamb,lobster,pork,shrimp)
com_fre = data.frame(matrix(ncol=3, nrow=0), stringsAsFactors = FALSE)
x<-c("stars", "meat", "frequency")
colnames(com_fre) <- x

######## Step 2: Created Data Frame for Word Frequency ########

#a.Get Rid of Words Appearing Less Than 10 Times on Reviews
ch_filtered = ch_filtered[ch_filtered$word %in% names(which(table(ch_filtered$word) > 10)), ]

#b.Calculated Word Frequency Under Different Levels of Stars
for (i in meat){
  for (j in 1:5){
   a = nrow(com_meats[com_meats$word == i & com_meats$stars == j,])
   b = nrow(com_meats[com_meats$word == i,])
   freq = a/b
   c = paste("star",j,sep="")
   d = c(c,i,freq)
   com_fre[nrow(com_fre)+1,]<-d
  }
}

#c.Find Associated Words
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

a = com_fre[com_fre$meat == "beef",]
b = com_fre[com_fre$meat == "chicken",]
c = com_fre[com_fre$meat == "crab",]
d = com_fre[com_fre$meat == "duck",]
e = com_fre[com_fre$meat == "frog",]
f = com_fre[com_fre$meat == "lamb",]
g = com_fre[com_fre$meat == "lobster",]
h = com_fre[com_fre$meat == "pork",]
i = com_fre[com_fre$meat == "shrimp",]
barplot(a,main="beef",xlab="stars",ylab="Frequency")
p1=ggplot(data=a,aes(x=stars,y=frequency))+ geom_bar(stat="identity")
p2=ggplot(data=b,aes(x=stars,y=frequency))+ geom_bar(stat="identity")
p3=ggplot(data=c,aes(x=stars,y=frequency))+ geom_bar(stat="identity")
p4=ggplot(data=d,aes(x=stars,y=frequency))+ geom_bar(stat="identity")
p5=ggplot(data=e,aes(x=stars,y=frequency))+ geom_bar(stat="identity")
p6=ggplot(data=f,aes(x=stars,y=frequency))+ geom_bar(stat="identity")
p7=ggplot(data=g,aes(x=stars,y=frequency))+ geom_bar(stat="identity")
p8=ggplot(data=h,aes(x=stars,y=frequency))+ geom_bar(stat="identity")
p9=ggplot(data=i,aes(x=stars,y=frequency))+ geom_bar(stat="identity")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=4)
