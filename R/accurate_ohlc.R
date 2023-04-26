accurate_ohlc <- function(dt_ohlc, na = NA, check_volume = NULL, check_adjusted = NULL) {
  checkmate::assert_data_frame(dt_ohlc)
  checkmate::assert_logical(na, len = 1L)
  checkmate::assert_flag(check_volume, null.ok = TRUE)
  checkmate::assert_flag(check_adjusted, null.ok = TRUE)
  nz <- dt_ohlc$open > 0 & dt_ohlc$high > 0 & dt_ohlc$low > 0 & dt_ohlc$close > 0
  h <- dt_ohlc$high >= dt_ohlc$open & dt_ohlc$high >= dt_ohlc$close
  l <- dt_ohlc$low <= dt_ohlc$open & dt_ohlc$low <= dt_ohlc$close
  acc <- nz & h & l

  if (is.null(check_volume)) check_volume <- "volume" %in% colnames(dt_ohlc)
  if (is.null(check_adjusted)) check_adjusted <- "adjusted" %in% colnames(dt_ohlc)
  if (check_volume) acc <- acc & dt_ohlc$volume >= 0
  if (check_adjusted) acc <- acc & dt_ohlc$adjusted > 0

  if (!is.na(na)) acc[is.na(acc)] <- na

  acc
}

prep_ohlc <- function(dt_ohlc) {
  checkmate::assert_data_frame(dt_ohlc)
  class <- class(dt_ohlc)[[1L]]
  if (!is.data.table(dt_ohlc)) dt_ohlc <- as.data.table(dt_ohlc)

  dt_ohlc <- dt_ohlc[accurate_ohlc(dt_ohlc, na = TRUE),]

  num_cols <- colnames(dt_ohlc)[sapply(dt_ohlc, is.numeric)]
  by <- if ("symbol" %in% colnames(dt_ohlc)) "symbol"
  dt_ohlc[, c(num_cols) := setnafill(.SD, type = "locf"), by = by, .SDcols = num_cols]
  dt_ohlc <- na.omit(dt_ohlc)

  if (class == "tbl_df") return(tibble::as_tibble(dt_ohlc))
  if (class == "data.frame") return(setDF(dt_ohlc))
  tryCatch(as(dt_ohlc, class), error = function(e) dt_ohlc[])
}
