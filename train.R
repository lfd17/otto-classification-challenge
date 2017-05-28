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
library(nnet)
library(xgboost)

source('helpers.R')
source('classifiers.R')

# Collect the training data.
trainingData <- fread('data/train.csv')
trainingData$id <- NULL

# Collect the test data.
testData <- fread('data/test.csv')
testData$id = NULL

# naiveBayesClassifier(trainingData, testData)
# logisticRegressionClassifier(trainingData, testData)
decisionTreeClassifier(trainingData = trainingData, testData = testData)
# randomForestClassifier(trainingData, testData)
# neuralNetworkClassifier(trainingData, testData)


