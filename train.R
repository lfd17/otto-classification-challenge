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
source('helpers.R')
source('classifiers.R')

# Collect the training data.
trainingData <- fread('data/train.csv')
trainingData$id <- NULL
targets <- trainingData$target
trainingData$target <- NULL

# Collect the test data.
testData <- fread('data/test.csv')
testData$id = NULL

# result <- knn(train = trainingData, test = testData, cl=targets, k = 3)
# randomForestClassifier(trainingData, testData)
