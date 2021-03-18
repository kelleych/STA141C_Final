#' @import purrr
#' @import furrr
#' @import stats
#' @import parallel
#' @import tidyverse
#' @import future
#' @importFrom magrittr %>%
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))
library(future)
library(purrr)
library(furrr)

suppressWarnings(plan(multiprocess, workers = 2))

#' Preform bootstrapping linear regression
#' @param formula A formula
#' @param data A data frame
#' @param parallelization A boolean value on whether or not you want to use parallelization
#' @param m An int that is the upper bound of indexes of your data
#' @param B An int that tells the function how many times you want to bootstrap
#' @return res An list of the bootstrapped models
#' @export
blblm <- function(formula, data, parallelization=FALSE, m = 10, B = 5000) {
  set.seed(10)
  data_list <- split_data(data, m)
  if(parallelization == TRUE){
    estimates <- future_map(data_list, ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  }
  else{
    estimates <- map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  }
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

blblg <- function(formula, data, parallelization=FALSE, m = 10, B = 5000) {
  set.seed(10)
  data_list <- split_data(data, m)
  if(parallelization == TRUE){
    estimates <- future_map(data_list, ~ lg_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  }
  else{
    estimates <- map(
      data_list,
      ~ lg_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  }
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblg"
  invisible(res)
}



blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
blbsigma <- function(fit) {
  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}



mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

future_map_mean <- function(.x, .f, ...){
  (future_map(.x, .f, ...) %>% reduce(`+`))/length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

future_map_cbind <- function(.x, .f, ...){
  future_map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}

future_map_rbind <- function(.x, .f, ...){
  future_map(.x, .f, ...) %>% reduce(rbind)
}

