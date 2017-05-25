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

source('helpers.R')
source('classifiers.R')

# Collect the training data.
trainingData <- fread('data/train.csv')
trainingData$id <- NULL

# Collect the test data.
testData <- fread('data/test.csv')
testData$id = NULL

# Write to output file.
#id <- rownames(prediction)
#prediction <- cbind(id=id, prediction)
#prediction <- data.table(prediction)
#res <- prediction[, print(.SD)]

#fwrite(prediction, file = "data/submission_randomForest.csv")
