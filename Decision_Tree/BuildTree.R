rm(list=ls())

library(rpart)
library(rpart.plot)
library(caTools)
library(pROC)
library(ROSE)
library(randomForest)
library(RWeka)
library(caret)
library(party)


# Load cleaned data
data <- read.csv('./cleanedData.csv', stringsAsFactors = T)
data$lead <- as.factor(data$lead)


# Split into test and training data
set.seed(1)
sample <- sample.split(Y=data$lead, SplitRatio = 0.8)
data_train <- data[sample,]
test <- data[!sample,]

table(train$lead)

# Over/Under Sampling
# To get balanced values
# under -> N = 560
# over -> N = 1220
# both -> N = 600
train_adjusted <- ovun.sample(lead ~ ., 
                          data=data_train, 
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



####testing new models

set.seed(1005566)#for reproducibility

levels(data_train$lead)=c("Yes","No")# 1=Yes, 0=No

train<- createFolds(data$lead,k=10)


##CART Models
#rpart algorithm 
C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='rpart', cp=0.04) #acc:68

#rpart2 algorithm
C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='rpart2', metric='Accuracy', preProcess='center',
                tuneGrid=data.frame(maxdepth=14),tuneLength=5, trControl = trainControl(method='cv',number=5)) #68

#rpart2 algorithm
#C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='rpart2', metric='Accuracy', preProcess='center',
                #tuneGrid=data.frame(maxdepth=14),tuneLength=5, trControl = trainControl(method='cv',number=5)) #68


##bagged CART
#treebag algorithm
#C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=train_adjusted, method='treebag',
#tuneLength=10, trControl = trainControl(method='cv',number=7)) #acc:89

C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=train_adjusted, method='treebag') #acc:89


##C4.5 like tree model
#J48 algorithm
C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='J48', tuneGrid=data.frame(C=0.1,M=1),
                tuneLength=10,trControl = trainControl(method='cv',number=7)) #acc:79


C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='J48', tuneGrid=data.frame(C=0.01,M=3),
                tuneLength=5,trControl = trainControl(method='repeatedcv',number=10)) #acc:68

##C5.0 model
#C5.0 algorithm
C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=train_adjusted, method='C5.0',trials=5, model='tree',winnow=FALSE,
                tuneLength=2, trControl = trainControl(method='optimism_boot',number=3,classProbs = TRUE,sampling = 'up')) #acc:75


##random forest 
#random forest algorithm

C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=train_adjusted, method='rf',
                tuneLength=7, trControl = trainControl(method='cv',number=7,classProbs = TRUE,sampling = 'up')) #acc:89


C45Fit
C45Fit$finalModel

# Check Accuracy
# Model Accuracy 
#levels(test$lead)=c("Yes","No")
predTest <- predict(C45Fit, test)

probTest <- predict(C45Fit, test, type="prob")
actualTest <- test$lead

# Adding variables
test$actual <- actualTest
test$pred <- predTest
test$prob1 <- probTest[,2]
test$prob0 <- probTest[,1]
test$prob <- ifelse(test$prob0 > test$prob1, test$prob0, test$prob1)

confusionMatrix(test$pred,test$actual)

