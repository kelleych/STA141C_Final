#' Get the sigma value for the model
#' @param object A model
#' @param parallelization A boolean value on whether or not you want to use parallelization
#' @param confidence A boolean value on whether or not you want a confidence value
#' @param level A double that is the confidence level you want
#' @return sigma A double
#' @export sigma.blblm
sigma.blblm <- function(object, parallelization = FALSE,confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  if(parallelization){
    sigma <- mean(future_map_dbl(est, ~mean(future_map_dbl(., "sigma"))))
  }
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    if(parallelization){      limits <- est %>% future_map(~quantile(future_map_dbl(., "sigma"), c(alpha/2,1-alpha/2)))%>% set_names(NULL)
    }
    else{
      limits <- est %>%
        map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
        set_names(NULL)
    }
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  }
  else {
    return(sigma)
  }
}
