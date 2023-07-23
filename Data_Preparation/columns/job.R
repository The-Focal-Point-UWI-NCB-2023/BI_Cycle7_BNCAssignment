
data <- data[data$job!="",]
data$job <- as.character(data$job)
data$job[data$job=='admin.'] <- "admin"
# Remove empty factor levels
data$job <- as.factor(data$job)
data$job <- droplevels(data$job)
levels(data$job)