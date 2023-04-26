regress <- function(data, formula, ...) {
  stats::lm(formula, data = data, ...)
}
