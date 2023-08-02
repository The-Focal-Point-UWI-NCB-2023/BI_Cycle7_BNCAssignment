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

set.seed(1)

levels(data_train$lead)=c("Yes","No")# 1=Yes, 0=No

train<- createFolds(data$lead,k=10)

C45Fit <- train(lead ~ agerange + job + marital + education + balance + deposit, data=data_train, method='rpart2', metric='Accuracy', preProcess=c("center", "scale",'nzv','zv'),
                tuneGrid=data.frame(maxdepth=10),tuneLength=3, trControl = trainControl(method='repeatedcv',number=3,repeats=4,sampling = 'smote')) #train: 62, test:71

C45Fit

# Check Accuracy
# Model Accuracy 
levels(test$lead)=c("Yes","No")
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

