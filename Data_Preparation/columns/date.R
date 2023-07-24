#Checking For Empty Rows
empty_rows <- is.na(data$date)
sum(empty_rows)

#Checking For Spaces
space_in_row <- grepl("\\s", data$date)
sum(space_in_row)

=



data$date <- as.Date(data$date, format = "%m/%d/%Y")
data[is.na(data$date)]
summary(data)




month.name
#check if a month is invalid using is Date
months = 1:12
names(months) = month.name

data$date <- as.Date(paste(months[str_to_title(data$month)], data$day, sep = "-"), format = "%m-%d")


# Check and print rows with NA values in a specific column FIRST CHECK
na_rows <- is.na(data$date)

# Print the rows with NA values
for (i in which(na_rows)) {
  cat("Row", i, "contains an NA value in column", "column_name", "\n")
}

for (i in 1:nrow(data)) {
  if(is.na(data$date[i])){
    fixed.date <- as.Date(paste(months[str_to_title(data$month[i])], 1, sep = "-"), format = "%m-%d")
    end.of.month <- lubridate::ceiling_date(fixed.date, "month") - 1
    last.day <- format(end.of.month,format ='%d')
    data$day[i] <- as.integer(last.day)
    data$date[i] <- end.of.month
  }
  
  
}

# Check and print rows with NA values in a specific column FINAL CHECK
na_rows <- is.na(data$date)

# Print the rows with NA values
for (i in which(na_rows)) {
  cat("Row", i, "contains an NA value in column", "column_name", "\n")
}



if (sum(na_rows)== 0) {
  data$date <- NULL
  c("No errors in data and date column deleted")
}else{
  c("Manually Recheck Script and DataSet")
}




















