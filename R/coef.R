#' Get the coefficients of the model
#' @param object A model
#' @param parallelization A boolean value on whether or not you want to run parallelization
#' @return the coefficients of the model
#' @export coef.blblm
coef.blblm <- function(object,parallelization = FALSE,...) {
  est <- object$estimates
  if(parallelization){
    future_map_mean(est, ~future_map_cbind(., "coef") %>% rowMeans())
  }
  else{
    map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
  }
}
