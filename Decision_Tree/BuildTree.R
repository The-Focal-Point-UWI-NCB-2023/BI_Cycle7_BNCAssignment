rm(list=ls())

library(rpart)
library(rpart.plot)
library(caTools)
library(pROC)

# Load cleaned data
data <- read.csv('./cleanedData.csv')
#View(data)
data$lead <- as.factor(data$lead)
data$agerange <- as.factor(data$agerange)


# Split into test and training data
set.seed(1)
sample <- sample.split(Y=data, SplitRatio = 0.8)
train <- data[sample,]
test <- data[!sample,]

## Build Model
DTmodel <- rpart(
  lead ~ agerange + job + marital + education + balance + deposit,
  data = train,
  method = "class",
  minsplit = 50, # Min records to split
  minbucket = 30, # Min records at a node
  maxdepth = 10,
  cp = -1,
  parms = list(split="information")
)

# 50, 30, 30 -> 68%

DTmodel

rpart.plot(DTmodel, 
           type = 5, # What is displayed at each node 
           extra = 101, # Values displayed at the leaf node
           fallen.leaves = F, # Get tree looking structure
           cex = 0.6) # Font size

# Check Accuracy
# Model Accuracy 
predTest <- predict(DTmodel, test, type="class")
probTest <- predict(DTmodel, test, type="prob")
actualTest <- test$lead

t1 <- table(Predicted_Value = predTest, Actual_Value = actualTest)
t1

accuracy1 <- sum(diag(t1))/sum(t1)
accuracy1
