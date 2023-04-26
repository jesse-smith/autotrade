download_sp_index <- function(index = c("SP400", "SP500", "SP600"), batch_size = 20L, from = as.Date("1990-01-01"), to = Sys.Date(), ...) {
  rlang::arg_match(index, multiple = TRUE)
  index <- unique(index)
  holdings <- data.table::setDT(index_holdings(index))[, c("index", "symbol")]
  dt <- download_bulk(holdings$symbol, batch_size = batch_size, from = from, to = to, ...)
  dt <- data.table::merge.data.table(holdings, dt, by = "symbol", all.x = TRUE)
  data.table::setcolorder(dt, "index")
  setTBL(dt)
  dt
}

download_bulk <- function(symbols, batch_size = 20L, from = as.Date("1990-01-01"), to = Sys.Date(), ...) {
  symbols <- unique(symbols)
  n_sym <- length(symbols)
  n_bat <- ceiling(n_sym / batch_size)
  batch <- rep(seq_len(n_bat), each = batch_size)
  batch <- batch[seq_len(n_sym)]
  p <- progressr::progressor(n_sym)
  dt_lst <- furrr::future_map(seq_len(n_bat), function(i, ...) {
    s <- symbols[batch == i]
    dt <- data.table::setDT(tidyquant::tq_get(s, from = from, to = to, ...))
    p(amount = length(s))
    dt
  }, ...)
  data.table::rbindlist(dt_lst)
}

index_holdings <- function(index = c("SP400", "SP500", "SP600")) {
  index <- rlang::arg_match(
    index,
    values = tidyquant::tq_index_options(),
    multiple = TRUE
  )
  names(index) <- index
  holdings <- index |>
    purrr::map(~ data.table::setDT(tidyquant::tq_index(.x))) |>
    data.table::rbindlist(idcol = "index") |>
    setTBL()
  holdings
}
