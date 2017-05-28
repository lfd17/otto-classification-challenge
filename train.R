# Clean the environment.
rm(list = ls());gc()

library(data.table)
library(reshape2)
library(dplyr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(class)
library(e1071)

source('helpers.R')
source('classifiers.R')

# Collect the training data.
trainingData <- fread('data/train.csv')
trainingData$id <- NULL

# Collect the test data.
testData <- fread('data/test.csv')
testData$id = NULL

# result <- randomForestClassifier(trainingData, testData)

# result <- decisionTreeClassifier(trainingData = trainingData, testData = testData)

# Calculate error rate.
res <- getTrainingTestDatasets(trainingData)
train <- res[[1]]
test <- res[[2]]
testClasses <- res[[3]]
fit <- rpart(target ~ ., method = "class", data = train)

# Predict the test data classes.
prediction <- predict(fit, newdata = test)
error <- calculateErrorRate(prediction, testClasses)
print(sprintf("Error rate is %f", error))


fit <- naiveBayes(target ~ ., data = trainingData)
prediction <- predict(fit, newdata = testData, type = "raw")
pdt <- data.table(prediction)
names <- colnames(pdt)
pdt$predictedClass = pdt[,names[apply(.SD,1,which.max)]]
