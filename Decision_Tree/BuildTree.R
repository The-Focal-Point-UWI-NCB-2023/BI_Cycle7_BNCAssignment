library(rpart)
library(rpart.plot)
library(caTools)
library(pROC)

# Load cleaned data
data <- read.csv('./cleanedData.csv')
View(data)
data$lead <- as.factor(data$lead)


# Split into test and training data
set.seed(1)
sample <- sample.split(Y=data$lead, SplitRatio = 0.8)
train <- data[sample,]
test <- data[!sample,]

## Build Model
DTmodel <- rpart(
  lead ~ education + age + marital + job + deposit + balance,
  method = "class", 
  data = train,
  parms = list(split = "information"), 
  control = rpart.control(
    minsplit = 1, # Min records to split
    minbucket = 20, # Min records at a node
    maxdepth = 8 # Prep-runnning: Control tree size
  )
)

DTmodel

rpart.plot(DTmodel, 
           type = 5, # What is displayed at each node 
           extra = 101, # Values displayed at the leaf node
           fallen.leaves = F, # Get tree looking structure
           cex = 0.6) # Font size


predTest <- predict(DTmodel, test, type="class")
summary(predTest)

##old code

# Split into test and training data
set.seed(1)
sample <- sample.split(data$lead, SplitRatio = 0.7)

training_data <- subset(data, sample == TRUE)
test_data <- subset(data, sample == FALSE)

# Checking that split ratio is preserved
sampleN <- nrow(data[data$lead==0,])
sampleY <- nrow(data[data$lead==1,])

trainN <- nrow(training_data[training_data$lead==0,])
trainY <- nrow(training_data[training_data$lead==1,])

testN <- nrow(test_data[test_data$lead==0,])
testY <- nrow(test_data[test_data$lead==1,])
print(cat(sampleN/sampleY, trainN/trainY, testN/testY))
