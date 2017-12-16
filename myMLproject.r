library(lattice)
library(rpart.plot)
library(caret)
library(rpart)
library(ggplot2)
library(caret)
library(gbm)
# library(rattle)
library(randomForest)
# Only Random Forest Examplified

# load data locally
training <- read.csv("pml-training.csv", na.strings = c("NA", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA", ""))

training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

trainData <- training[, -c(1:7)]
testData <- testing[, -c(1:7)]

set.seed(1249) 
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
train <- trainData[inTrain, ]
valid <- trainData[-inTrain, ]

fit_rf <- train(classe ~ ., data = train, method = "rf")
print(fit_rf)

predict_rf <- predict(fit_rf, valid)
predict_final  <- predict(fit_rf, testData)
