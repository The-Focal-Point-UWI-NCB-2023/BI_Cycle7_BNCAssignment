#Intial look at data
str(data)
summary(data)
View(data)

# Detecting Outliers
balancePreClean <- data$balance
d <- density(balancePreClean,na.rm = T)
hist(balancePreClean)
plot(d,frame=FALSE, col = "Blue",main="Balance")
boxplot(balancePreClean)

#Cleaning
data$old.balance <- data$balance
avg.job.balance <- aggregate(balance ~ job, data, mean, na.rm = TRUE)
data$avg.balance <- avg.job.balance$balance[match(data$job, avg.job.balance$job)]
#Replace balance with average balance based on job if the balance is over the 75% quater, because we don't want to affect more than 10% of the value
#and we would not be affecting up to 10% when we change outliers using this method
data$balance <- ifelse(data$balance > 1500 , as.integer(data$avg.balance), data$balance)
data$new.balance <- data$balance
data$ratio <- percent((data$new.balance- data$old.balance)/data$old.balance)
data <- data %>% mutate(ratio = (old.balance- new.balance) / old.balance,
                        percentage = percent(ratio),
                        new.deposit = ifelse(ratio == 0, deposit, deposit * (1-ratio)))

data$new.balance <- NULL
data$old.balance <- NULL
data$ratio <- NULL
data$percentage <- NULL
data$avg.balance <- NULL
data$deposit <- data$new.deposit
data$new.deposit <- NULL
avg.job.balance <- NULL
data$balance <- ifelse(data$balance<=-500,-500, data$balance)
data$deposit[is.na(data$deposit)] <- 0

#count <- sum(data$balance > 668520)
#print(paste("Number of rows where balance is higher than 668520:", count))



#After Cleaning 
data$deposit <- as.integer(data$deposit)
data$deposit <- as.numeric(data$deposit)

d <- density(data$balance,na.rm = T)
hist(data$balance)
plot(d,frame=FALSE, col = "Blue",main="Balance")
boxplot(data$balance)