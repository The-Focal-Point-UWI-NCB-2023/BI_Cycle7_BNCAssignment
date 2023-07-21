library(ggplot2)
#check for na and null values 
str(data$month)
month_na_sum <- sum(is.na(data$month))
month_null_sum <- sum(is.null(data$month))

#create a list of valid months
valid_months <- factor(c(tolower(month.name)))

#check if any months are invalid
invalid_months <- !(tolower(data$month) %in% valid_months)
num_of_invalid <- sum(invalid_months)


sum_deposits_per_month <- aggregate(deposit ~ month, data, sum)

ggplot(sum_deposits_per_month, aes(x = month, y = deposit, label=deposit)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_bar(data = subset(sum_deposits_per_month, deposit == max(deposit)), fill = "red", stat = "identity")+
  geom_text(size = 3, position = position_stack(vjust=1.02)) +
  xlab("Months") +
  ylab("Amount of Deposits") +
  ggtitle("Amount of Deposits per month") +
  theme(plot.title = element_text(hjust = 0.5))