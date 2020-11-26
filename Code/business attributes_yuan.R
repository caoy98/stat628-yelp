library(jsonlite)
business = stream_in(file("data/business_city.json"))

##########  tests and models of attributes  ############

chinese = business[grep("Chinese", business$categories), ]
all_attributes = chinese$attributes
attributes_names = colnames(all_attributes)

for (i in c(4,18)) {
  chinese$attributes[, i] = gsub("^u", "", chinese$attributes[, i])
}

for (i in 1:ncol(all_attributes)) {
  chinese$attributes[, i] = gsub("None", NA, chinese$attributes[, i])
  chinese$attributes[, i] = as.factor(chinese$attributes[, i])
}

for (i in c(2,3,4,9,10)) {
  plot = ggplot(data = subset(chinese, !is.na(attributes[, i])), aes(x=attributes[, i], y=stars, fill=attributes[, i])) + 
    geom_boxplot(alpha=0.8) + 
    theme(plot.title = element_text(color = "black", size = 12, face = "bold"), legend.position = "none") +
    labs(x = attributes_names[i], y = "Stars") +
    ggtitle(paste("Boxplot of Attribute", attributes_names[i]))
  print(plot)
  #ggsave(paste("attribute_", attributes_names[i], ".png", sep = ""), dpi = 300)
}
