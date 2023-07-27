rm(list=ls())

library(rpart)
library(rpart.plot)
library(caTools)
library(pROC)
library(ROSE)

# Load cleaned data
data <- read.csv('./cleanedData.csv', stringsAsFactors = T)
data$lead <- as.factor(data$lead)


# Split into test and training data
set.seed(1)
sample <- sample.split(Y=data$lead, SplitRatio = 0.8)
train <- data[sample,]
test <- data[!sample,]

table(train$lead)

# Over/Under Sampling
# To get balanced values
# under -> N = 560
# over -> N = 1220
# both -> N = 600
train_adjusted <- ovun.sample(lead ~ ., 
                          data=train, 
                          method="over", 
                          seed = 1, 
                          N=1220)$data

table(train_adjusted$lead)

## Build Model
DTmodel <- rpart(
  lead ~ agerange + job + marital + education + balance + deposit,
  data = train_adjusted,
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

# Adding variables
test$actual <- actualTest
test$pred <- predTest
test$prob1 <- probTest[,2]
test$prob0 <- probTest[,1]
test$prob <- ifelse(test$prob0 > test$prob1, test$prob0, test$prob1)

# Confusion Matrix
cMatrix <- table(Predicted_Value = predTest, Actual_Value = actualTest)
cMatrix

Accuracy <- sum(diag(cMatrix))/sum(cMatrix)
Accuracy

# True Positive Rate
TPR <- cMatrix[4]/(cMatrix[4]+cMatrix[2])
TPR

# True Negative Rate
TNR <- cMatrix[1]/(cMatrix[1]+cMatrix[3])
TNR

# ROC & Area under the curve
ROC <- roc(actualTest, probTest[,2])
plot(ROC,col="blue")
AUC <- auc(ROC)
AUC
