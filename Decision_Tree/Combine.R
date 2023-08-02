rm(list=ls())

library(utils)

data <- read.csv('./cleanedData.csv', stringsAsFactors = T)
data$lead <- as.factor(data$lead)

# Create Combinations
age = levels(data$agerange)
job = levels(data$job)
marital = levels(data$marital)
education = levels(data$education)

combinations <- expand.grid(age, job, marital, education)

names(combinations)[names(combinations) == "Var1"] <- "agerange"
names(combinations)[names(combinations) == "Var2"] <- "job"
names(combinations)[names(combinations) == "Var3"] <- "marital"
names(combinations)[names(combinations) == "Var4"] <- "education"

# Fill in balance
avg.job.balance <- aggregate(balance ~ job, data, mean, na.rm = TRUE)
combinations$balance <- avg.job.balance$balance[match(combinations$job, avg.job.balance$job)]

# Fill in deposit
avg.job.deposit <- aggregate(deposit ~ job, data, mean, na.rm = TRUE)
combinations$deposit <- avg.job.deposit$deposit[match(combinations$job, avg.job.deposit$job)]
