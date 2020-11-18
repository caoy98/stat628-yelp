# read Chinese restaurants review file and load packages
library(tidytext); library(dplyr); library(wordcloud2); library(ggplot2); library(magrittr)
ch_review = read.csv("chinese_review.csv", stringsAsFactors = F)
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
undesirable_words = c("food", "chinese", "service", "restaurant", "chicken", 
                      "restaurants") # These words basically are in similar probabilities for all stars
taste_words = c("sweet", "sour", "salty", "spicy", "bitter")

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
