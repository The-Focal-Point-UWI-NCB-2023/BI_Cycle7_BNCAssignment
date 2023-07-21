# Convert to factor check for uniqueness
data$RefNum = as.factor(data$RefNum)

# Remove empty factor levels 
data$RefNum <- droplevels(data$RefNum)
levels(data$RefNum)