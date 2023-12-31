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
sample <- sample.split(Y=data$lead, SplitRatio = 0.9)
data_train <- data[sample,]
test <- data[!sample,]

# Over/Under Sampling
# To get balanced values
# under -> N = 560
# over -> N = 1220
# both -> N = 600
train_adjusted <- ovun.sample(lead ~ ., 
                              data=data_train, 
                              method="over", 
                              seed = 1, 
                              N=2000)$data

table(train_adjusted$lead)

## Build Model
DTmodel <- rpart(
  lead ~ agerange + job + marital + education + balance + deposit,
  data = data_train,
  method = "class",
  minsplit = 12, # Min records to split
  minbucket = 11, # Min records at a node
  maxdepth =7,
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

confusionMatrix(test$pred,test$actual)

###########################################KINDLY IGNORE################################

####testing new models

set.seed(1)

set.seed(1005566)#for reproducibility


levels(data_train$lead)=c("Yes","No")# 1=Yes, 0=No

train<- createFolds(data$lead,k=10)


##Alternatives

##CART Models

#rpart2 algorithm
###best one so far
C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='rpart2', metric='Accuracy', preProcess=c("center", "scale",'nzv','zv'),
                tuneGrid=data.frame(maxdepth=10),tuneLength=3, trControl = trainControl(method='repeatedcv',number=3,repeats=4,sampling = 'smote')) #train: 62, test:71


#rpart algorithm 
train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='rpart', metric='Accuracy', preProcess=c("center", "scale",'nzv','zv')
      ,tuneLength=5, trControl = trainControl(method='repeatedcv',number=3,repeats=4,sampling = 'smote'))

#1

#rpart2 algorithm
#C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='rpart2', metric='Accuracy', preProcess='center',
#tuneGrid=data.frame(maxdepth=14),tuneLength=5, trControl = trainControl(method='cv',number=5)) #68


##bagged CART
#treebag algorithm
#C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=train_adjusted, method='treebag',
#tuneLength=10, trControl = trainControl(method='cv',number=7)) #acc:89

C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='treebag',metric='Accuracy',preProcess=c("center",'nzv','zv'),
                tuneLength=5,trControl = trainControl(method='cv')) #acc:55, roc: 58


##C4.5 like tree model
#J48 algorithm
C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='J48',preProcess=c("center",'scale','nzv','zv'),
                tuneLength=5,trControl = trainControl(method='optimism_boot',number=3,sampling = 'smote')) #acc:69


C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='J48', tuneGrid=data.frame(C=0.01,M=3),
                tuneLength=5,trControl = trainControl(method='repeatedcv',number=10,repeats =3,sampling = 'smote' )) #acc:69, seed: 1, roc: 57

##C5.0 model
#C5.0 algorithm
C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='C5.0',
                tuneLength=10, trControl = trainControl(method='repeatedcv',number=3,repeats=2, sampling='smote')) #acc:67,seed:1,roc:58.83


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

# ROC & Area under the curve
ROC <- roc(actualTest, probTest[,2])
plot(ROC,col="blue")
AUC <- auc(ROC)
AUC


#----------------------------------------
install.packages('C50')
library(C50)

pred <- c('agerange','job','marital','education','balance','deposit')

set.seed(1)

# Load cleaned data
data <- read.csv('./cleanedData.csv', stringsAsFactors = T)
data$lead <- as.factor(data$lead)


# Split into test and training data
set.seed(1)

sample <- sample.split(Y=data$lead, SplitRatio = 0.9)
train <- data[sample,]
test <- data[!sample,]

table(train$lead)

# Over/Under Sampling
# To get balanced values
# under -> N = 560
# over -> N = 1220
# both -> N = 600
train<- ovun.sample(lead ~ ., 
                              data=train, 
                              method="over", 
                              seed = 1, 
                              N=1050)$data

mod <-C5.0(x=train[,pred],y=train$lead,trials=5,control = C5.0Control(CF=0.7, minCases = 35))
mod
summary(mod)

C5imp(mod, metric = "splits")

plot(mod,trial=1, subtree=NULL, type='simple')
predTest <- predict(mod, test,trials=3)
probTest <- predict(mod, test, type="prob",trials=3)
actualTest <- test$lead

# Adding variables
test$actual <- actualTest
test$pred <- predTest
test$prob1 <- probTest[,2]
test$prob0 <- probTest[,1]
test$prob <- ifelse(test$prob0 > test$prob1, test$prob0, test$prob1)

confusionMatrix(test$pred,test$actual)


# ROC & Area under the curve
ROC <- roc(actualTest, probTest[,2])
plot(ROC,col="blue")


AUC <- auc(ROC)
AUC


