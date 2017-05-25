findClasses <- function(data) {
  data %>% 
    group_by(target) %>%
    summarise(no_rows = length(target))
}


constructOutputFile <- function(prediction, outputFileName) {
  # Write to output file.
  id <- rownames(prediction)
  prediction <- cbind(id = id, prediction)
  prediction <- data.table(prediction)
  fwrite(prediction, file = outputFileName)
  sprintf("Submission output written to %s", outputFileName)
}