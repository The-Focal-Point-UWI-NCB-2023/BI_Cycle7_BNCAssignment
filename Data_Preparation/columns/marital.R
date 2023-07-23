#Viewing the number of unique values within the column (Should be 2 (Yes and NO))
length(unique(data$marital)) #= 3

#Counting Empty Spaces 
sum(data$marital == "") # 2 Empty Rows

#Counting Na values
sum(is.na(data$marital))
levels(data$marital)
str(data$marital)

#Cleaning Empty Spaces by deletion as there is no other option to infer the values and there are only 2 rows with empty values
data <- data[data$marital != "",]

length(unique(data$marital))

# Remove Empty Factor Levels
data$marital <- as.character(data$marital)
data$marital <- as.factor(data$marital)