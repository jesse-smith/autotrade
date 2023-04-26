#' Calculate Maximum Drawdown Size within Current Drawdown
#'
#' @param x `[numeric | xts]` A numeric vector or xts object containing price
#'   data. Cannot contain missing values or multiple columns
#' @return `[class(x)]` An object of the same class as `x` with maximum drawdowns
#'   at each value
#' @export
max_drawdown <- function(x) {
  # Error if multiple columns
  if (NCOL(x) > 1L) stop("`max_drawdown()` cannot handle objects with multiple columns")
  # Error if missing
  if (anyNA(x)) stop("`max_drawdown()` cannot handle missing values")
  # Just return if length less than 2
  if (NROW(x) == 0L) return(x[0L])
  if (NROW(x) == 1L) return(xts::reclass(0, x))
  # Calculate drawdowns
  dd <- cummax(x) - x
  # Get points where there is a drawdown (i.e. drawdown is not 0)
  i_dd <- which(dd != 0)
  # Return if all zero
  if (length(i_dd) == 0L) return(dd)
  # Create max drawdown variable
  dd_max <- dd
  for (i in i_dd) dd_max[[i]] <- max(c(dd_max[[i]], dd_max[[i - 1L]]))
  dd_max <- reclass(dd_max, dd)
  dd_max
}
