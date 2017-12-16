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

## Ignore some data which might not be useful
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
conf_rf <- confusionMatrix(valid$classe, predict_rf)
accuracy_rf <- conf_rf$overall[1]


####Sample Run and Final Output##########
#Random Forest 

#13737 samples
#52 predictor
#5 classes: 'A', 'B', 'C', 'D', 'E' 

#No pre-processing
#Resampling: Bootstrapped (25 reps) 
#Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 
#Resampling results across tuning parameters:
  
#  mtry  Accuracy   Kappa    
#2    0.9887293  0.9857383
#27    0.9887483  0.9857637
#52    0.9801501  0.9748835

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was mtry = 27.

#> predict_rf <- predict(fit_rf, valid)
#> predict_final  <- predict(fit_rf, testData)

#> conf_rf <- confusionMatrix(valid$classe, predict_rf)
#> accuracy_rf <- conf_rf$overall[1]
#> accuracy_rf
#Accuracy 
#0.9935429 

#> predict_final
#[1] B A B A A E D B A A B C B A E E A B B B
#Levels: A B C D E
