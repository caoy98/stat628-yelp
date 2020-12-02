# read Chinese restaurants review file and load packages
library(tidytext); library(dplyr); library(wordcloud2); library(ggplot2); library(tidyr)
ch_review = read.csv("chinese_review.csv", stringsAsFactors = F)

# undesirable_words and target taste words
undesirable_words = c("food", "chinese", "restaurant", "chicken", 
                      "restaurants") # These words basically are in similar probabilities for all stars
taste_words = c("sweet", "sour", "salty", "spicy", "bitter", "umami")
service_words = c("parking", "lot", "valet", "garage", "validated", "tip", "wifi", "service", "WIFI", "WiFi")

######### single words and bigrams (n*p matrix from Yuxiao) ##############

wordList <- c("sweet","sour","spicy","salty")
plotWordStar(stars = stars,DTM = review_word_df,wordList = wordList)

#########  words ############

# tokenization of review text and filter words
review_filtered = ch_review %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) >= 3) %>%
  filter(!word %in% undesirable_words)

# specific tastes
review_taste_filtered = review_filtered %>%
  filter(word %in% taste_words)

# word cloud for all words
review_words_counts = review_filtered %>%
  count(word, sort = T) %>%
  top_n(100)
wordcloud2(review_words_counts, size = 1)

# popular words for different stars
popular_words = review_filtered %>% 
  group_by(stars) %>%
  count(word, stars, sort = TRUE) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(stars,n) %>%
  mutate(row = row_number()) 

# popular taste words for stars
popular_taste_words = review_filtered %>% 
  group_by(stars) %>%
  count(word, stars, sort = TRUE) %>%
  mutate(prob = n/sum(n)) %>%
  ungroup() %>%
  arrange(word,stars) %>%
  filter(word %in% taste_words)
#saveRDS(popular_taste_words, file = "shinyapp/data/taste.rds")

# visualization for different stars
popular_words %>%
  ggplot(aes(row, n, fill = stars)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Popular Words by Chart Level") + 
  theme_text() +  
  facet_wrap(~stars, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, # notice need to reuse data frame
    labels = popular_words$word) +
  coord_flip()

popular_taste_words %>%
  ggplot(aes(y=prob, x=stars)) + 
  geom_bar(stat="identity") +
  ggtitle("Probability of Taste Words") +
  facet_wrap(~word) +
  theme_light() +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position="none") +
  labs(x = "Star", y = "Probability") 
  ggsave(filename = "Image/Tastes-DiffStars_Barplot.png", dpi = 300)


################ bigrams #######################

review_bigrams = ch_review %>%
  select(c(2,5,9,11)) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% undesirable_words) %>%
  filter(!word2 %in% undesirable_words) %>%
  unite(bigram, word1, word2, sep = " ")

saveRDS(popular_bigrams, file = "shinyapp/data/bigram_filtered.rds")

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