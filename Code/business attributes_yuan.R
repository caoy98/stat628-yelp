# load data and packages
library(ggplot2)
library(jsonlite)
library(MASS)
library(ordinal)
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
#saveRDS(attributes_star, file = "data/chinese.rds")

# boxplot for different attributes
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

##########  tests and models of attributes  ############

# attributes with 2 groups, Wilcoxon-Mann Whitney test
#wilcox.test(stars~GoodForKids, data = attributes_star) # significant (p=0.02419), 
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

attributes_star_mod = attributes_star
attributes_star_mod$stars = factor(attributes_star_mod$stars, ordered = T)

model = polr(stars~., Hess = T,
             data = na.omit(attributes_star_mod))
summary(model)

ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
ctable <- cbind(ctable, "p value" = p)
