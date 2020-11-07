# read JSON file
library(tidytext); library(dplyr); library(jsonlite)
review = stream_in(file("Data/review_city.json"))
business = stream_in(file("Data/business_city.json"))
