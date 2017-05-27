source('helpers.R')

# Classify the dataset with decision tree.
decisionTreeClassifier <- function(trainingData, testData) {
  outputFileName <- 'data/submission_decisionTree.csv'
  
  # Construct the model.
  fit <- rpart(target ~ ., method = "class", data = trainingData)
  print("Trained the model.")
  
  # Predict the test data classes.
  prediction <- predict(fit, newdata = testData)
  print("Predicted the results.")
  
  constructOutputFile(prediction, outputFileName)
}

# Classify the dataset using random forests.
randomForestClassifier <- function(trainingData, testData) {
  outputFileName <- 'data/submission_randomForest.csv'
  
  # 75% of the training dataset.
  sampleSize <- floor(0.75 * nrow(trainingData))
  
  # set the seed to make your partition reproductible
  set.seed(123)
  trainIndex <-
    sample(seq_len(nrow(trainingData)), size = sampleSize)
  
  # Construct the train and test datasets.
  train <- trainingData[trainIndex,]
  test <- trainingData[-trainIndex,]
  testClasses <- test$target
  test$target = NULL
  
  minError <- .Machine$double.xmax
  bestTreeCount <- 10
  
  numberOfTrees <- c(10, 25, 50, 100, 250)
  for (treeCount in numberOfTrees) {
    # Fit the model with given tree count.
    fit <-
      randomForest(
        as.factor(target) ~ .,
        data = train,
        importance = TRUE,
        ntree = treeCount
      )
    
    # Predict the class values.
    prediction <- predict(fit, newdata = test)

    # Calculate the error.
    test$originalClass <- testClasses
    test$predictedClass <- prediction
    test$result <- test[, result <- originalClass == predictedClass]
    errorRate <- table(test$result)["FALSE"] / length(test$result)

    print(sprintf("Error rate with %d trees is %f.", treeCount, errorRate))

    if (errorRate < minError) {
      minError <- errorRate
      bestTreeCount <- treeCount
    }
  }
  
  print(sprintf("Best fitting values are %d trees with error rate of %f.",
          bestTreeCount,
          minError))
  
  # Fit the model with best tree count.
  fit <-
    randomForest(
      as.factor(target) ~ .,
      data = trainingData,
      importance = TRUE,
      ntree = bestTreeCount
    )
  
  # Predict the class values.
  prediction <- predict(fit, newdata = testData, type = "prob")
  constructOutputFile(prediction, outputFileName)
}
