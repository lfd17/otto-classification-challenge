findClasses <- function(data) {
  data %>% 
    group_by(target) %>%
    summarise(no_rows = length(target))
}