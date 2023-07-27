rm(list=ls())

library(rpart)
library(rpart.plot)
library(caTools)
library(pROC)

# Load cleaned data
data <- read.csv('./cleanedData.csv')
#View(data)
data$lead <- as.factor(data$lead)


# Split into test and training data
set.seed(1)
sample <- sample.split(Y=data$lead, SplitRatio = 0.8)
train <- data[sample,]
test <- data[!sample,]

## Build Model
DTmodel <- rpart(
  lead ~ age + job + marital + education + balance,
  data = train,
  method = "class",
  parms = list(split="gini"), # Information gain
  minsplit = 80, # Min records to split
  minbucket = 40, # Min records at a node
  maxdepth = 8,
  cp = -1
)

DTmodel

rpart.plot(DTmodel, 
           type = 5, # What is displayed at each node 
           extra = 101, # Values displayed at the leaf node
           fallen.leaves = F, # Get tree looking structure
           cex = 0.6) # Font size


predTest <- predict(DTmodel, test, type="class")
summary(predTest)
