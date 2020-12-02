rm(list=ls())
# load packages
library(jsonlite)
library(tidytext)
library(dplyr)
library(stringr)
library(plyr)
library(wordcloud2)
library(ggplot2)
library(magrittr)
library(tidyr)
library(rlist)
library(gridExtra)
####### Select Reviews of Chinese Restaurants and Add Business Info ########

# load required package for json and read data
review = stream_in(file("Data/review_city.json"))
business = stream_in(file("Data/business_city.json"))

# add some business information to review file
nm = c("categories", "city", "state", "latitude", "longitude")
review[nm] = lapply(nm, function(x) business[[x]][match(review$business_id, business$business_id)])

# select reviews with categories including "Chinese"
chinese_business = business[grep("Chinese", business$categories), ]
chinese_review = review[grep("Chinese", review$categories), ]

# write to json and csv file
write_json(chinese_review, path = "chinese_review.json")
write.csv(chinese_review, file = "chinese_review.csv", row.names = F)

####### Pre-processing and Words Related with Service #######

rm(list=ls())

original_data <- read.csv("chinese_review.csv", header = TRUE, sep=",")
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

wordList <- c("waiting","takeout","parking","wifi","menu","toilet")
plotWordStar(stars = stars,DTM = review_word_df,wordList = wordList)

####### Words Related with Meat #######

######## Step1: Loading Data and Select Key Words ########
detach("package:plyr")

#a.read Chinese restaurants review file and load packages
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

####### Words Related with Taste #######

ch_review = read.csv("chinese_review.csv", stringsAsFactors = F)

# undesirable_words and target taste words
taste_words = c("sweet", "sour", "salty", "spicy", "bitter", "umami")

######### single words ########

wordList <- c("bitter","sweet","sour","spicy","salty","umami")
plotWordStar(stars = stars,DTM = review_word_df,wordList = wordList)

#########  words  ########

# tokenization of review text and filter words
review_filtered = ch_review %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) >= 3)

# specific tastes
review_taste_filtered = review_filtered %>%
  filter(word %in% taste_words)

# word cloud for all words
review_words_counts = review_filtered %>%
  count(word, sort = T) %>%
  top_n(100)
wordcloud2(review_words_counts, size = 1)

# popular taste words by probability for stars
popular_taste_words = review_filtered %>% 
  group_by(stars) %>%
  count(word, stars, sort = TRUE) %>%
  mutate(prob = n/sum(n)) %>%
  ungroup() %>%
  arrange(word,stars) %>%
  filter(word %in% taste_words)
saveRDS(popular_taste_words, file = "shinyapp/data/taste.rds")

# popular service words
popular_service_words = review_service_filtered %>% 
  group_by(stars) %>%
  count(word, stars, sort = TRUE) %>%
  mutate(prob = n/sum(n)) %>%
  ungroup() %>%
  arrange(word,stars) %>%
  filter(word %in% service_words)
saveRDS(popular_service_words, file = "shinyapp/data/service.rds")

popular_taste_words %>%
  ggplot(aes(y=prob, x=stars)) + 
  geom_bar(stat="identity") +
  ggtitle("Probability of Taste Words") +
  facet_wrap(~word) +
  theme_light() +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position="none") +
  labs(x = "Star", y = "Probability") 
ggsave(filename = "Image/Tastes-DiffStars_Barplot.png", dpi = 300)

# significance of "spicy"
spicy_review = ch_review
spicy_review$spicy = grepl("spicy", spicy_review$text)
wilcox.test(stars~spicy, data = spicy_review)
spicy_mod = MASS::polr(factor(stars, ordered = T)~spicy, data = spicy_review, Hess = T)
summary(spicy_mod)
# calculate p value
ctable_spicy <- coef(summary(spicy_mod))
p <- pnorm(abs(ctable_spicy[, "t value"]), lower.tail = FALSE) * 2
ctable_spicy <- cbind(ctable_spicy, "p value" = p)
# calculate odds ratio
coef(spicy_mod) %>% exp()

######### bigrams ###########

undesirable_words = c("food", "chinese", "restaurant", "chicken", 
  "restaurants") # These words basically are in similar probabilities for all stars

review_bigrams = ch_review %>%
  select(c(2,5,9,11)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% undesirable_words) %>%
  filter(!word2 %in% undesirable_words) %>%
  unite(bigram, word1, word2, sep = " ")

saveRDS(review_bigrams, file = "shinyapp/data/bigram_filtered.rds")

popular_bigrams = review_bigrams %>% 
  group_by(stars) %>%
  count(bigram, stars, sort = TRUE) %>%
  mutate(prob = n/sum(n)) %>%
  top_n(n=100, wt = prob) %>%
  ungroup() %>%
  arrange(stars,prob)

popular_bigrams %>%
  ggplot(aes(row, n, fill = stars)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Popular Words by Chart Level") + 
  theme_classic() +  
  facet_wrap(~stars, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_bigrams$row, # notice need to reuse data frame
    labels = popular_bigrams$bigram) +
  coord_flip()

####### Business Attributes Test and Model ########

# load data and packages
library(MASS) # ordinal logistic regression
business = stream_in(file("Data/business_city.json"))

# find all Chinese restaurants
chinese = business[grep("Chinese", business$categories), ]
all_attributes = chinese$attributes
attributes_names = colnames(all_attributes)

# cleaning for attributes
for (i in 1:ncol(all_attributes)) {
  all_attributes[, i] = gsub("^u", "", all_attributes[, i])
  all_attributes[, i] = gsub("None", NA, all_attributes[, i])
  all_attributes[, i] = as.factor(all_attributes[, i])
}

# combine stars and attributes, delete attributes with too many NAs
attributes_star = cbind(chinese$business_id, chinese$stars, all_attributes[, -c(1,8,19,20:39)])
colnames(attributes_star)[1:2] = c("business_id", "stars")
saveRDS(attributes_star, file = "shinyapp/data/chinese.rds")

# boxplot for some attributes
for (i in c(2,3,4,9,10)) {
  plot = ggplot(data = subset(chinese, !is.na(attributes[, i])), aes(x=attributes[, i], y=stars, fill=attributes[, i])) + 
    geom_boxplot(alpha=0.8) + 
    theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position = "none") +
    labs(x = attributes_names[i], y = "Stars") +
    ggtitle(paste("Boxplot of Attribute", attributes_names[i]))
  print(plot)
  #ggsave(paste("attribute_", attributes_names[i], ".png", sep = ""), dpi = 300)
}

# boxplot for states
ggplot(data = subset(chinese, !is.na(state)), aes(x=state, y=stars, fill=state)) + 
  geom_boxplot(alpha=0.8) + 
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position = "none") +
  labs(x = "States", y = "Stars") +
  ggtitle("Boxplot of States")

## tests and models

# attributes with 2 groups, Wilcoxon-Mann Whitney test
wilcox.test(stars~GoodForKids, data = attributes_star) # significant (p=0.02419), 
wilcox.test(stars~BusinessAcceptsCreditCards, data = attributes_star) # significant (p=0.03357)
wilcox.test(stars~ByAppointmentOnly, data = attributes_star) # significant (p=0.002514)
wilcox.test(stars~OutdoorSeating, data = attributes_star) # significant (p=0.03864)
wilcox.test(stars~RestaurantsReservations, data = attributes_star) # significant (p=1.482e-05)
#wilcox.test(stars~RestaurantsGoodForGroups, data = attributes_star) # not significant (p=0.088)
#wilcox.test(stars~WheelchairAccessible, data = attributes_star) # not significant (p=0.73)

# attributes with multigroups, Kruskal Wallis
kruskal.test(stars~RestaurantsPriceRange2, data = attributes_star) # significant (p=0.01485)
kruskal.test(stars~NoiseLevel, data = attributes_star) # significant (p=0.0199)
kruskal.test(stars~WiFi, data = attributes_star) # significant (p=0.04848)
#kruskal.test(stars~Alcohol, data = attributes_star) # not significant (0.7773)
#kruskal.test(stars~state, data = chinese) # not significant (0.238)

# ordinal logistic regression
## choose significant attributes
attributes_star_mod = attributes_star[, c(2,4:7,9,15,18)]
attributes_star_mod[, -c(3,8)] = sapply(attributes_star_mod[, -c(3,8)], as.numeric)
attributes_star_mod[, -c(3,8)] = attributes_star_mod[, -c(3,8)]-1
# give NoiseLevel and WiFi variables an order
attributes_star_mod$NoiseLevel = factor(attributes_star_mod$NoiseLevel, levels = c("'quiet'", "'average'", "'loud'", "'very_loud'"), ordered = T)
attributes_star_mod$NoiseLevel = as.numeric(attributes_star_mod$NoiseLevel)
attributes_star_mod$WiFi = factor(attributes_star_mod$WiFi, levels = c("'paid'", "'no'", "'free'"), ordered = T)
attributes_star_mod$WiFi = as.numeric(attributes_star_mod$WiFi)
# change stars as ordered factor
attributes_star_mod$stars = factor(attributes_star_mod$stars, ordered = T)

# fit ordinal logistic regression
model = MASS::polr(stars ~ ., Hess = T, data = attributes_star_mod)
summary(model)
# calculate p value
ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
# calculate odds ratio
coef(model) %>% exp()
