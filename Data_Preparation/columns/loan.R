#Checking For Empty Rows
empty_rows <- is.na(data$loan)
sum(empty_rows)

#Checking For Spaces
space_in_row <- grepl("\\s", data$loan)
sum(space_in_row)

#Check if all rows have the correct category
all(data$loan == "yes" | data$loan == "no")


