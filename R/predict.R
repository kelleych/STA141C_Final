#' Get predictions for the model
#' @param object A model
#' @param parallelization A boolean for whether or not you want to run parallelization
#' @param new_data A dataframe of data you want to predict
#' @param confidence A boolean of whether or not you want a confidence interval
#' @param level A double that is the confidence interval
#' @return prediction values
#' @export predict.blblm
predict.blblm <- function(object, parallelization = FALSE,new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    if(parallelization){
      future_map_mean(est, ~future_map_cbind(., ~ X %*% .$coef) %>% apply(1, mean_lwr_upr, level=level) %>% t())
    }
    else{
      map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
                 apply(1, mean_lwr_upr, level = level) %>%
                 t())
    }
  } else {
    if(parallelization){
      future_map_mean(est, ~ future_map_cbind(., ~ X %*% .$coef) %>% rowMeans())
    }
    else{
      map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
    }
  }
}
