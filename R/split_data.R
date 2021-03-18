#' split data into m parts of approximated equal sizes
#'

library(dplyr)

split_data <- function(data, m) {
  set.seed(10)
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}
