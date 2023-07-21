#Convert to characters so that we can convert it to date
data$temp <- as.character(data$month)
data$temp2 <- as.character(data$day)
data$date <- as.Date(paste("2019", data$temp, data$temp2, sep = "-"), format = "%Y-%B-%d")
#Subtracting current date from last deposit to get the days from last deposit
data$last_deposit <- as.integer(Sys.Date() - data$date)
#Clean up temp columns used for calculation
data$temp <- NULL
data$temp2 <- NULL