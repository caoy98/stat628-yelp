# load required package for json and read data
library(jsonlite)
review = stream_in(file("Data/review_city.json"))
business = stream_in(file("Data/business_city.json"))

# select reviews of Chinese food
chinese = business[grep("Chinese", business$categories), ]
chineseid = business$business_id[grep("Chinese", business$categories)]
chineseind = grep(paste(chineseid, collapse = '|'), review$business_id)
chinese_review = review[chineseind, ]

# add category to review
chinese_review$category = chinese_review$business_id
for (i in 1:length(chineseid)) {
  chinese_review$category = gsub(chineseid[i], chinese$categories[i], chinese_review$category)
}

# write to json and csv file
write_json(chinese_review, path = "chinese_review.json")
write.csv(chinese_review, file = "chinese_review.csv")
