findClasses <- function(data) {
  data %>% 
    group_by(target) %>%
    summarise(no_rows = length(target))
}


constructOutputFile <- function(prediction, outputFileName) {
  # Write to output file.
  prediction <- data.table(prediction)
  id <- rownames(prediction)
  prediction <- cbind(id = id, prediction)
  fwrite(prediction, file = outputFileName)
  print(sprintf("Submission output written to %s", outputFileName))
}

getTrainingTestDatasets <- function(trainingData) {
  
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
  
  output <- list(train, test, testClasses)
  return(output)
}

calculateErrorRate <- function(prediction, originalClasses) {
  
  # Get the dominant class from the probability matrix.
  prediction <- data.table(prediction)
  names <- colnames(prediction)
  prediction$predictedClass = prediction[,names[apply(.SD,1,which.max)]]
  
  # Calculate the error.
  prediction$originalClass <- originalClasses
  prediction$result <- prediction[, result <- originalClass == predictedClass]
  errorRate <- table(prediction$result)["FALSE"] / length(prediction$result)
  
  return(errorRate)
}
