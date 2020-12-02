# load required package for json and read data
library(jsonlite)
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
