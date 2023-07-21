#Viewing the number of unique values within the column (Should be 2 (Yes and NO))
length(unique(data$housing)) #= 3

#Counting Empty Spaces 
sum(data$housing == "")

#Counting Na values
sum(is.na(data$housing))

#Cleaning Empty Spaces by deletion as there is no other option to infer the values
data <- data[data$housing != "",]

# Remove empty factor levels
data$housing <- droplevels(data$housing)
levels(data$housing)

