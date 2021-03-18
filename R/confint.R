#' Get the confidence interval for the model
#' @param object A model
#' @param parallelization A boolean value for whether or not you want to run parallelization
#' @param level A double that is the confidence interval level
#' @return out A list of confidence intervals for each variable
#' @export confint.blblm
confint.blblm <- function(object,parallelization = FALSE, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  if(parallelization){
    out <- future_map_rbind(parm, function(p){
      future_map_mean(est, ~future_map_dbl(., list("coef",p)) %>% quantile(c(alpha/2, 1-alpha/2)))
    })
  }
  else{
    out <- map_rbind(parm, function(p) {
      map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
    })
  }
  if (is.vector(out)){
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}
