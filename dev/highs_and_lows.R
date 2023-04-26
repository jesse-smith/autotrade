qlib(tidyquant, tidyverse, timetk)
theme_set(ggthemes::theme_fivethirtyeight())
theme_replace(axis.title = element_text())
progressr::handlers(global = TRUE)
options("rc.cores" = parallel::detectCores() - 1L, "mc.cores" = parallel::detectCores() - 1L)

# Use S&P large, mid, and small cap indices for testing
# All 3 have data from 1989-01-01 onward
data <- tq_get(c("LABU"), from = "1980-01-01") |>
  prep_ohlc() |>
  select(-c(volume, adjusted)) |>
  # Remove ^ from symbols and group
  group_by(symbol) |>
  # Add price ratios
  add_ratios() |>
  # Add lags
  tk_augment_lags(-date, 1:5) |>
  # Remove equal highs and lows
  filter(!cumall(hl_inter == lh_inter)) |>
  mutate(symbol = factor(symbol)) |>
  drop_na()


# Strategy
# Attempt to buy at today's low
# Attempt to sell at tomorrow's high
# Needed predictions:
#   Entry point: predicted daily low
#   Exit point: predicting daily high (or predicted return from entry?)
# Extensions:
#   Optimize entry point based on expected returns

# Distribution of minimum and maximum theoretical returns from a buy-sell cycle
data |>
  select(symbol, date, min_return = hl_inter, max_return = lh_inter) |>
  pivot_longer(ends_with("return"), names_to = "type", names_pattern = "(min|max)_return", values_to = "return") |>
  ggplot(aes(x = return, color = type)) +
  geom_density(bw = "SJ") +
  scale_color_discrete(type = c("steelblue", "firebrick")) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(-0.25, 0.5)) +
  facet_grid("symbol", scales = "free_y")

# Entry point prediction

# Initial data
data_ol_intra <- as.data.frame(select(data, -c("high", "low", "close"), -"lh_intra", -matches("[ohc]_intra$"), -matches("[hlc]_inter$")))
data_hl_inter <- as.data.frame(select(data, -c("high", "low", "close"), -matches("_intra$"), -matches("hc_inter$"), -matches("[olc]l_inter$")))
data_ll_inter <- as.data.frame(select(data, -c("high", "low", "close"), -matches("_intra$"), -matches("hc_inter$"), -matches("[ohc]l_inter$")))
data_cl_inter <- as.data.frame(select(data, -c("high", "low", "close"), -matches("_intra$"), -matches("hc_inter$"), -matches("[ohl]l_inter$")))
# Variable selection
data_sel_ol_intra <- var_select_ranger(ol_intra, data = data_ol_intra, mtry = function(n) trunc(n / 3), splitrule = "maxstat")
data_sel_hl_inter <- var_select_ranger(hl_inter, data = data_hl_inter, mtry = function(n) trunc(n / 3), splitrule = "maxstat")
data_sel_ll_inter <- var_select_ranger(ll_inter, data = data_ll_inter, mtry = function(n) trunc(n / 3), splitrule = "maxstat")
data_sel_cl_inter <- var_select_ranger(cl_inter, data = data_cl_inter, mtry = function(n) trunc(n / 3), splitrule = "maxstat")
# Model fitting
rf_ol_intra <- ranger::ranger(
  ol_intra ~ ., data = data_sel_ol_intra,
  mtry = function(n) trunc(n / 3),
  splitrule = "maxstat",
  quantreg = TRUE,
  keep.inbag = TRUE
)
rf_hl_inter <- ranger::ranger(
  hl_inter ~ ., data = data_sel_hl_inter,
  mtry = function(n) trunc(n / 3),
  splitrule = "maxstat",
  quantreg = TRUE,
  keep.inbag = TRUE
)
rf_ll_inter <- ranger::ranger(
  ll_inter ~ ., data = data_sel_ll_inter,
  mtry = function(n) trunc(n / 3),
  splitrule = "maxstat",
  quantreg = TRUE,
  keep.inbag = TRUE
)
rf_cl_inter <- ranger::ranger(
  cl_inter ~ ., data = data_sel_cl_inter,
  mtry = function(n) trunc(n / 3),
  splitrule = "maxstat",
  quantreg = TRUE,
  keep.inbag = TRUE
)

# Predictions
q_by <- 0.1
q <- seq(q_by, 1 - q_by, q_by)
pred_ol_intra <- predict(rf_ol_intra, type = "quantiles", quantiles = q)$predictions |>
  as_tibble() |>
  rename_with(~ paste0("ol_intra", 100 * as.numeric(str_extract(.x, "[.0-9]+")))) |>
  mutate(
    symbol = data$symbol,
    date = data$date,
    ol_intra_mu = predict({{ rf_ol_intra }}, data = {{ data_ol_intra }})$predictions
  ) |>
  mutate(across(-c("symbol", "date"), ~ data$open * (.x + 1)))
pred_hl_inter <- predict(rf_hl_inter, type = "quantiles", quantiles = q)$predictions |>
  as_tibble() |>
  rename_with(~ paste0("hl_inter", 100 * as.numeric(str_extract(.x, "[.0-9]+")))) |>
  mutate(
    symbol = data$symbol,
    date = data$date,
    hl_inter_mu = predict({{ rf_hl_inter }}, data = {{ data_hl_inter }})$predictions
  ) |>
  mutate(across(-c("symbol", "date"), ~ data$high_lag1 * (.x + 1)))
pred_ll_inter <- predict(rf_ll_inter, type = "quantiles", quantiles = q)$predictions |>
  as_tibble() |>
  rename_with(~ paste0("ll_inter", 100 * as.numeric(str_extract(.x, "[.0-9]+")))) |>
  mutate(
    symbol = data$symbol,
    date = data$date,
    ll_inter_mu = predict({{ rf_ll_inter }}, data = {{ data_ll_inter }})$predictions
  ) |>
  mutate(across(-c("symbol", "date"), ~ data$low_lag1 * (.x + 1)))
pred_cl_inter <- predict(rf_cl_inter, type = "quantiles", quantiles = q)$predictions |>
  as_tibble() |>
  rename_with(~ paste0("cl_inter", 100 * as.numeric(str_extract(.x, "[.0-9]+")))) |>
  mutate(
    symbol = data$symbol,
    date = data$date,
    cl_inter_mu = predict({{ rf_cl_inter }}, data = {{ data_cl_inter }})$predictions
  ) |>
  mutate(across(-c("symbol", "date"), ~ data$close_lag1 * (.x + 1)))

data_low <- select(data, -c("high", "close"), -matches("_intra$"), -matches("[hlc]_inter$"))
data_sel_low <- var_select_ranger(low, data = data_low, mtry = function(n) trunc(n / 3), splitrule = "maxstat")
data_sel_low$symbol <- data_low$symbol
data_sel_low$date <- data_low$date
data_pred_low <- pred_ol_intra |>
  left_join(pred_hl_inter, by = c("symbol", "date")) |>
  left_join(pred_ll_inter, by = c("symbol", "date")) |>
  left_join(pred_cl_inter, by = c("symbol", "date")) |>
  relocate(symbol, date) |>
  left_join(data_sel_low, by = c("symbol", "date"))


data_sel_pred_low <- var_select_ranger(low, data = data_pred_low, mtry = function(n) trunc(n / 3), splitrule = "maxstat")
rf_low <- ranger::ranger(
  low ~ ., data = data_sel_pred_low,
  mtry = function(n) trunc(n / 3),
  splitrule = "maxstat",
  quantreg = TRUE,
  keep.inbag = TRUE
)

q_low <- predict(rf_low, type = "quantiles", quantiles = seq(0.01, 0.99, 0.01))$predictions |>
  as_tibble() |>
  rename_with(~ paste0("low", 100 * as.numeric(str_extract(.x, "[.0-9]+")))) |>
  mutate(symbol = data$symbol, date = data$date, low = data$low)

calc_return <- function(low, high, entry, exit) {
  r <- numeric(length(high))
  curr_action <- "buy"
  for (i in seq.int(2L, length(high), 2L)) {
    if (curr_action == "buy" && entry[[i]] >= low[[i]]) {
      entry_price <- entry[[i]]
      curr_action <- "sell"
    } else if (curr_action == "sell" && exit[[i]] <= high[[i]]) {
      r[[i]] <- exit[[i]] / entry_price - 1
      curr_action <- "buy"
    }
  }
  r
}

ret <- data |>
  mutate(
    entry = q_low$low60,
    exit = close,
    return_max = calc_return(low, high, low, high),
    return = calc_return(low, high, entry, exit)
  ) |>
  select(symbol, date, return, return_max) |>
  drop_na()

tq_performance(ret, Ra = return, performance_fun = table.AnnualizedReturns)
tq_performance(ret, Ra = return, performance_fun = table.DownsideRisk)

ret |>
  mutate(return = cumprod(return + 1), return_max = cumprod(return_max + 1)) |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = return)) +
  scale_y_log10()
table.Drawdowns(xts(ret$return, ret$date), top = 10)
table.DownsideRisk(xts(ret$return, ret$date))
table.DrawdownsRatio(xts(ret$return, ret$date))
table.AnnualizedReturns(xts(ret$return, ret$date))
