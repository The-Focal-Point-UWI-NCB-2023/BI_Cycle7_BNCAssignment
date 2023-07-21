#Take an initial look at data
str(data)
summary(data)

# Detecting Outliers
depositPreClean <- data$deposit
d <- density(depositPreClean,na.rm = T)
hist(depositPreClean)
plot(d,frame=FALSE, col = "Blue",main="Deposit")
boxplot(depositPreClean)


#Using average deposit based on job
avg.job.deposit <- aggregate(deposit ~ job, data, mean, na.rm = TRUE)
data$avg.deposit <- avg.job.deposit$deposit[match(data$job, avg.job.deposit$job)]
data$deposit <- ifelse(is.na(data$deposit), round(data$avg.deposit,1), round(data$deposit,1))
data$deposit <- ifelse(data$deposit>190000, 190000, data$deposit)

#After Cleaning
data$avg.deposit <- NULL
avg.job.deposit <- NULL
avg.job.balance <- NULL
data$deposit <- as.integer(data$deposit)
data$deposit <- as.numeric(data$deposit)
plot(density(data$deposit))
plot(hist(data$deposit))
str(data)
summary(data)
d <- NULL
