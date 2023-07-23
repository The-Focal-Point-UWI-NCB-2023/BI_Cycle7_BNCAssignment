library(caTools)

# Load cleaned data
data <- read.csv('./cleanedData.csv')

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