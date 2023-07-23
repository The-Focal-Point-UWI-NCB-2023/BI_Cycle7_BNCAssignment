#Remove empty factor levels
data$product <- as.factor(data$product)
data$product <- droplevels(data$product)
levels(data$product)