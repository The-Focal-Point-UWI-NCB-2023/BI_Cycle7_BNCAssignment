#Checking the structure of the data as well as missing values

str(data$won) # 2 Values 1 and 0
length(unique(data$won)) # No additional unseen values found
sum(is.na(data$won)) # No na values in the feature
summary(data$won) # Feature is clean 

## No cleaning or preparation required. 
