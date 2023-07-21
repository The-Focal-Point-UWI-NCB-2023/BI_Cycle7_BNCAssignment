# Age - Ordinal?
# Converting to factor, gives 42 unique values
summary(data)
data$temp = as.factor(data$age)
data$temp <- NULL

# Remove age 0s
data <- data[data$age>20,]

# ageSum <- as.vector(summary(data$age))
# ageMin <- ageSum[1]
# age1Q <- ageSum[2]
# ageMedian <- ageSum[3]
# ageMean <- ageSum[4]
# age3Q <- ageSum[5]
# ageMax <- ageSum[6]

# Outlier Detection
agePreClean <- data$age
plot(density(agePreClean))
plot(hist(agePreClean))

# Values > 60 Cap at 60
data$age[data$age > 60] <- 60

# Post Clean Visualization
plot(density(data$age))
plot(hist(data$age))

# Discretize Age (Using Equal Frequency Binning)
L <- min(data$age) # 20
H <- max(data$age) # 60
N <- 4 # 4 Groups from Histogram
IW <- ((H-L)/N) # 10

data$temp <- data$age

# Convert to groups as factor
age.groups <- c("20-29", "30-39", "40-49", "50-60") 
data$age <- as.factor(ifelse(
  data$temp < 60,
  age.groups[floor(((data$temp/10) - 1))], age.groups[4]
))

# Visualization (Before|After)
hist(data$temp, main = "Age Before Discritization")
hist(data$age, main = "Age After Discritization")

data$temp <- NULL