# read Chinese restaurants review file and load packages
library(tidytext); library(dplyr); library(wordcloud2); library(ggplot2); library(magrittr); library(tidyr)
ch_review = read.csv("chinese_review.csv", stringsAsFactors = F)
ch_review$stars = as.factor(ch_review$stars)
theme_text = function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

# undesirable_words and target taste words
undesirable_words = c("food", "chinese", "restaurant", "chicken", 
                      "restaurants") # These words basically are in similar probabilities for all stars
taste_words = c("sweet", "sour", "salty", "spicy", "bitter")

######### single words (n*p matrix) ##############

simple_review = ch_review %>% select(review_id, stars, text, category)

simple_review_filtered = simple_review %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) > 10) %>%
  filter(!word %in% undesirable_words) %>%
  fastDummies::dummy_cols(select_columns = "word") %>%
  select(-c(5:21)) %>%
  select(-word)

onestar = filter(simple_review_filtered, simple_review_filtered$stars==1) %>%
  group_by(review_id, stars) %>%
  summarise(across(starts_with('word'), sum))

twostar = filter(simple_review_filtered, simple_review_filtered$stars==2) %>%
  group_by(review_id, stars) %>%
  summarise(across(starts_with('word'), sum))

threestar = filter(simple_review_filtered, simple_review_filtered$stars==3) %>%
  group_by(review_id, stars) %>%
  summarise(across(starts_with('word'), sum))

fourstar = filter(simple_review_filtered, simple_review_filtered$stars==4) %>%
  group_by(review_id, stars) %>%
  summarise(across(starts_with('word'), sum))

fivestar = filter(simple_review_filtered, simple_review_filtered$stars==5) %>%
  group_by(review_id, stars) %>%
  summarise(across(starts_with('word'), sum))

final_review_filtered = rbind(onestar, twostar, threestar, fourstar, fivestar)


#########  words ###############

# tokenization of review text and filter words
review_filtered = ch_review %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(nchar(word) > 10) %>%
  filter(!word %in% undesirable_words)

# specific tastes
review_taste_filtered = review_filtered %>%
  filter(word %in% taste_words)

# plot of word counts for all stars
review_filtered %>%
  count(word, sort = T) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Prince Lyrics") +
  coord_flip()

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
popular_taste_words = review_taste_filtered %>% 
  group_by(stars) %>%
  count(word, stars, sort = TRUE) %>%
  slice(seq_len(10)) %>%
  ungroup() %>%
  arrange(stars,n) %>%
  mutate(row = row_number()) 

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
  ggplot(aes(row, n, fill = stars)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Popular Words by Chart Level") + 
  theme_text() +  
  facet_wrap(~stars, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_taste_words$row, # notice need to reuse data frame
    labels = popular_taste_words$word) +
  coord_flip()


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


popular_bigrams = review_bigrams %>% 
  group_by(stars) %>%
  count(bigram, stars, sort = TRUE) %>%
  mutate(prob = n/sum(n)) %>%
  top_n(n=10, wt = prob) %>%
  ungroup() %>%
  arrange(stars,prob) %>%
  mutate(row = row_number()) 

popular_bigrams %>%
  ggplot(aes(row, prob, fill = stars)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ylim(0,0.015) +
  ggtitle("Popular Words by Chart Level") + 
  theme_text() +  
  facet_wrap(~stars, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_bigrams$row, # notice need to reuse data frame
    labels = popular_bigrams$bigram) +
  coord_flip()
