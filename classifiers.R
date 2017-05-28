source('helpers.R')

# Classify the dataset with decision tree.
decisionTreeClassifier <- function(trainingData, testData) {
  
  # Split the training data into train and validation sets.
  res <- getTrainingTestDatasets(trainingData)
  train <- res[[1]]
  test <- res[[2]]
  testClasses <- res[[3]]
  
  # Train the model.
  fit <- rpart(target ~ ., method = "class", data = train[, -1])
  
  # Predict the test data classes.
  prediction <- predict(fit, newdata = test)
  error <- calculateErrorRate(prediction, testClasses)
  print(sprintf("Error rate is %f", error))
  
  # Train the model with the actual training data and predict on test dataset.
  fit <- rpart(target ~ ., method = "class", data = trainingData[, -1])
  prediction <- predict(fit, newdata = testData)
  
  constructOutputFile(prediction, "data/submission_decisionTree.csv")
}

# Classify the dataset using random forests.
randomForestClassifier <- function(trainingData, testData) {

  res <- getTrainingTestDatasets(trainingData)
  train <- res[[1]]
  test <- res[[2]]
  testClasses <- res[[3]]
  
  minError <- .Machine$double.xmax
  bestTreeCount <- 10
  
  numberOfTrees <- c(10, 25, 50, 100, 250)
  for (treeCount in numberOfTrees) {
    # Fit the model with given tree count.
    fit <-
      randomForest(
        as.factor(target) ~ .,
        data = train[, -1],
        importance = TRUE,
        ntree = treeCount
      )
    
    # Predict the class values and calculate the error.
    prediction <- predict(fit, newdata = test, type = "prob")
    errorRate <- calculateErrorRate(prediction, testClasses)
    print(sprintf("Error rate with %d trees is %f.", treeCount, errorRate))

    if (errorRate < minError) {
      minError <- errorRate
      bestTreeCount <- treeCount
      bestPrediction <- prediction
    }
  }
  
  print(sprintf("Best fitting values are %d trees with error rate of %f.",
          bestTreeCount,
          minError))
  
  # Fit the model with given tree count.
  fit <-
    randomForest(
      as.factor(target) ~ .,
      data = trainingData[, -1],
      importance = TRUE,
      ntree = treeCount
    )
  
  # Predict the class values.
  prediction <- predict(fit, newdata = testData, type = "prob")
  
  # Create the output file.
  constructOutputFile(prediction, "data/submission_randomForest.csv")
}

naiveBayesClassifier <- function(trainingData, testData) {
  
  res <- getTrainingTestDatasets(trainingData)
  train <- res[[1]]
  test <- res[[2]]
  testClasses <- res[[3]]
  
  print("Calculating the error rate of the algorithm.")
  fit <- naiveBayes(target ~ ., data = train[, -1])
  prediction <- predict(fit, newdata = test, type = "raw")
  errorRate <- calculateErrorRate(prediction, testClasses)
  print(sprintf("Error rate is %f.", errorRate))
  
  # Calculate the fit on all training data.
  fit <- naiveBayes(target ~ ., data = trainingData[, -1])
  print("Trained the model.")
  
  # Predict the test data classes.
  prediction <- predict(fit, newdata = testData, type = "raw")
  print("Predicted the results.")
  
  # Construct the output file.
  constructOutputFile(prediction, "data/submission_naiveBayes.csv")
}

logisticRegressionClassifier <- function(trainingData, testData) {
  
  # Calculate error rate.
  res <- getTrainingTestDatasets(trainingData)
  train <- res[[1]]
  test <- res[[2]]
  testClasses <- res[[3]]
  
  fit <- multinom(target ~ ., data = train[, -1])
  
  # Predict the test data classes.
  prediction <- predict(fit, newdata = test, type = "probs")
  errorRate <- calculateErrorRate(prediction, testClasses)
  print(sprintf("Error rate is %f.", errorRate))
  
  # Predict the test data classes.
  fit <- multinom(target ~ ., data = trainingData[, -1])
  prediction <- predict(fit, newdata = testData, type = "probs")
  constructOutputFile(prediction, "data/submission_logisticRegression.csv")
  
}

neuralNetworkClassifier <- function(trainingData, testData) {
  
  # Get training and validation sets.
  res <- getTrainingTestDatasets(trainingData)
  train <- res[[1]]
  test <- res[[2]]
  testClasses <- res[[3]]
  
  # Test the algorithm.
  fit <- nnet(as.factor(target) ~ ., data=train, size=8, na.action = na.omit, maxit = 1000, decay=5e-4)
  
  # Predict the test data classes and calculate the error rate.
  prediction <- predict(fit, newdata = test, type = "raw")
  errorRate <- calculateErrorRate(prediction, testClasses)
  print(sprintf("Error rate is %f.", errorRate))
  
  # Construct the test results.
  fit <- nnet(as.factor(target) ~ ., data=trainingData[, -1], size=8, na.action = na.omit, maxit = 1000, decay=5e-4)
  prediction <- predict(fit, newdata = testData, type = "raw")
  constructOutputFile(prediction, "data/submission_neuralNetwork_1000iterations_size8.csv")
}