library(shiny)
library(shinydashboard)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(dplyr)
library(wordcloud2)

chinese_business = readRDS("data/chinese.rds")
food_prob = read.csv("data/com_fre.csv")
taste_prob = readRDS("data/taste.rds")
service_prob = readRDS("data/service.rds")
word_filtered = readRDS("data/word_filtered.rds")
bigram_filtered = readRDS("data/bigram_filtered.rds")

meat_plot = ggplot(food_prob, aes(fill=as.factor(stars), y=frequency, x=stars)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Probability") +
  facet_wrap(~meat) +
  theme_ipsum() +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position="none") +
  xlab("Star") +
  ylab("Probability")

taste_plot = ggplot(taste_prob, aes(fill=as.factor(stars), y=prob, x=stars)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Probability") +
  facet_wrap(~word) +
  theme_ipsum() +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position="none") +
  xlab("Star") +
  ylab("Probability")

service_plot = ggplot(service_prob, aes(fill=as.factor(star), y=prob, x=star)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Probability") +
  facet_wrap(~word) +
  theme_ipsum() +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position="none") +
  xlab("Star") +
  ylab("Probability")
  