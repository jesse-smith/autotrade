online_trend <- function(x) {
  not_na <- which(!is.na(x))
  n_x <- length(x)
  n_not_na <- length(not_na)
  if (n_not_na == 0L) return(x)
  if (n_not_na == n_x) return(otrend_(x))
  x_full <- rep(NA_real_, n_x)
  x_full[not_na] <- otrend_(x[not_na])
  x_full
}
