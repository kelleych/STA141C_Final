#' Print blblm
#' @param x A model
#' @export print.blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}
