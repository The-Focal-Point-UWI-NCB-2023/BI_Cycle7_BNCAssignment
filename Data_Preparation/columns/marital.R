#Viewing the number of unique values within the column (Should be 2 (Yes and NO))
length(unique(data$marital)) #= 3

#Counting Empty Spaces 
sum(data$marital == "")

#Counting Na values
sum(is.na(data$marital))

#No cleaning Required


# Remove Empty Factor Levels
data$marital <- as.character(data$marital)
data$marital <- as.factor(data$marital)