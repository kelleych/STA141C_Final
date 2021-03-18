lm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- lm.wfit(X, y, freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}

#' compute the regression estimates for a blb dataset
lg1 <- function(formula, data, n) {
  environment(formula) <- environment()
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(data))))
  suppressWarnings(fit <- glm(formula, data, family = "binomial", weights = freqs)
  )
  list(coef = coef(fit), sigma = sigma(fit))
}
