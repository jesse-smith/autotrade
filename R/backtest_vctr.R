#' Backtest Positions in a Single Security Using Daily OHLCV Data
#'
backtest_vctr <- function(dt_ohlcvp, initial_cash = 1000L, in_place = FALSE) {
  # Check and convert inputs
  initial_cash <- as.double(checkmate::assert_number(initial_cash, lower = 0, finite = TRUE))
  dt <- std_ohlcv(dt_ohlcvp, position = TRUE, in_place = in_place)

  # Add column to track position changes
  set(dt, j = "position_change", value = c(0L, diff(dt$position)))
  # Add column for cash value at close
  set(dt, j = "cash", value = numeric(n))
  # Add column for stock shares at close (negative means short)
  set(dt, j = "shares", value = numeric(n))
  # Add column for total account value at close (negative means in debit)
  set(dt, j = "value", value = numeric(n))
  # Return if n == 0
  if (n == 0L) {
    set(dt, j = "return", value = numeric())
    return(dt)
  }

  # Perform first update with `initial_cash`
  init <- c(initial_cash, 0)
  if (dt$position[[1L]] == 1L) init <- rev(init) / dt$close[[1L]]
  init <- as.list(c(init, initial_cash))
  set(dt, i = 1L, j = c("cash", "shares", "value"), value = init)
  # Return if n == 1
  if (n == 1L) {
    set(dt, j = "return", value = 0)
    return(dt)
  }

  # Perform subsequent updates in loop
  for (i in seq.int(2L, n, 1L)) {
    if (dt$position_change[[i]] == 0L) {
      set(
        dt, i = i, j = c("cash", "shares"),
        value = dt[i - 1L, c("cash", "shares")]
      )
    } else if (dt$position_change[[i]] == 1L) {
      set(
        dt, i = i, j = c("cash", "shares"),
        value = list(0, dt$cash[[i - 1L]] / dt$close[[i]])
      )
    } else if (dt$position_change[[i]] == -1L) {
      set(
        dt, i = i, j = c("cash", "shares"),
        value = list(dt$shares[[i - 1L]] * dt$close[[i]], 0)
      )
    }
    set(
      dt, i = i, j = "value",
      value = dt$cash[[i]] + dt$shares[[i]] * dt$close[[i]]
    )
  }

  # Calculate % return
  set(dt, j = "return", value = dt$value / shift(dt$value) - 1)
  set(dt, i = 1L, j = "return", value = 0)

  # Return
  dt[]
}


std_ohlcv <- function(data, position = NULL, in_place = FALSE) {
  # Check inputs
  checkmate::assert_flag(in_place)
  assert_ohlcv(data, position)
  # Convert to data table
  dt <- if (in_place) setDT(data) else as.data.table(data)
  # Ensure columns are precisely the expected class
  if (!is.double(dt$open)) set(dt, j = "open", value = as.double(dt$open))
  if (!is.double(dt$high)) set(dt, j = "high", value = as.double(dt$high))
  if (!is.double(dt$low)) set(dt, j = "low", value = as.double(dt$low))
  if (!is.double(dt$close)) set(dt, j = "close", value = as.double(dt$close))
  if (!is.double(dt$volume)) set(dt, j = "volume", value = as.double(dt$volume))
  if (is.null(position)) position <- "position" %in% colnames(dt)
  if (position && !is.integer(position)) set(dt, j = "position", value = as.integer(dt$position))
  dt[]
}


assert_ohlcv <- function(data, position = NULL) {
  checkmate::assert_flag(position, null.ok = TRUE)
  checkmate::assert_data_frame(data)
  checkmate::assert_date(data$date, any.missing = FALSE)
  checkmate::assert_numeric(
    data$open, lower = 0, finite = TRUE, any.missing = FALSE
  )
  checkmate::assert_numeric(
    data$high, lower = 0, finite = TRUE, any.missing = FALSE
  )
  checkmate::assert_numeric(
    data$low, lower = 0, finite = TRUE, any.missing = FALSE
  )
  checkmate::assert_numeric(
    data$close, lower = 0, finite = TRUE, any.missing = FALSE
  )
  checkmate::assert_numeric(
    data$volume, lower = 0, finite = TRUE, any.missing = FALSE
  )
  if (is.null(position)) position <- "position" %in% colnames(data)
  if (position) {
    checkmate::assert_integerish(
      data$position, lower = 0L, upper = 1L, any.missing = FALSE
    )
  }
  stopifnot(all(data$high >= data$low))
  stopifnot(all(data$high >= data$open))
  stopifnot(all(data$high >= data$close))
  stopifnot(all(data$low <= data$open))
  stopifnot(all(data$low <= data$close))
  invisible(data)
}
