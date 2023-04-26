qlib(tidyverse)
theme_set(ggthemes::theme_fivethirtyeight())
theme_replace(axis.title = element_text())
progressr::handlers("progress")
progressr::handlers(global = TRUE)
future::plan("multisession", workers = future::availableCores() %/% 2L)

dt <- download_sp_index()
qs::qsave(dt, paste0("inst/extdata/sp_", gsub("-", "", Sys.Date()), ".qs"))
dt <- qs::qread(paste0("inst/extdata/sp_", gsub("-", "", Sys.Date()), ".qs"))

dt |>
  dplyr::group_by(symbol) |>
  dplyr::summarize(index = index[[1L]], date_min = min(date)) |>
  dplyr::arrange(index, date_min) |>
  dplyr::count(index, date_min) |>
  dplyr::group_by(index) |>
  dplyr::mutate(n = cumsum(n), p = n / max(n)) |>
  dplyr::ungroup() |>
  ggplot(aes(x = date_min, y = p, color = index)) +
  geom_step() +
  scale_y_continuous(limits = c(0, NA), labels = scales::label_percent()) +
  ggthemes::scale_color_fivethirtyeight()


strat_prep_rank <- function(dt, width = 63L, n = 10L, initial_equity = 1e4) {
  # Prepare data table
  dt <- data.table::as.data.table(dt)
  data.table::setkeyv(dt, c("symbol", "date"))
  # Score individual stocks based on returns (rank 1 to 100 and divide by total available on that date)
  dt[, score := 100 * rank(adjusted / shift(adjusted, fill = adjusted[[1L]])) / .N, by = "date"]
  # Get rolling scores
  dt[, roll_score := roll::roll_mean(score, ..width, weights = seq_len(..width), online = FALSE, min_obs = 1L), by = "symbol"]
  # Rank based on rolling scores
  dt[, weight := rank(roll_score), by = "date"]
  # Ensure weights are not missing and sum to 1
  dt[weight < 0 | is.na(weight) | is.infinite(weight), weight := 0]
  dt[, weight := weight / sum(weight), by = "date"]
  # Scale to initial equity
  dt[weight < 0 | is.na(weight) | is.infinite(weight), weight := 0]
  dt[, weight := ..initial_equity * weight, by = "date"]
  dt[weight < 0 | is.na(weight) | is.infinite(weight), weight := 0]
  # Return
  dt[]
}

cast_weight <- function(dt_prep) {
  dt_weight <- data.table::dcast.data.table(dt_prep, date ~ symbol, value.var = "weight")
  data.table::setnafill(dt_weight, type = "locf")
  data.table::setnafill(dt_weight, type = "nocb")
  data.table::setnafill(dt_weight, type = "const", fill = 0)
  dt_weight[, "date" := as.numeric(date)]
  as.matrix(dt_weight)
}

cast_price <- function(dt_prep, value_var = "adjusted") {
  dt_price <- data.table::dcast.data.table(dt_prep, date ~ symbol, value.var = value_var)
  data.table::setnafill(dt_price, type = "locf")
  data.table::setnafill(dt_price, type = "nocb")
  data.table::setnafill(dt_price, type = "const", fill = 0)
  dt_price[, "date" := as.numeric(date)]
  as.matrix(dt_price)
}

dt_prep <- strat_prep_rank(dt, initial_equity = 9e3)
mat_price <- cast_price(dt_prep)
mat_weight <- cast_weight(dt_prep)

# Doesn't work
dt_res <- rsims::fixed_commission_backtest(
  mat_price,
  mat_weight
)

dt_res |>
  group_by(Date) |>
  summarize(Value = sum(Value, na.rm = TRUE)) |>
  ggplot(aes(x = Date, y = Value)) +
  geom_step()
