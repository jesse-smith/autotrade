add_ratios <- function(dt_ohlc, inter_lag = 1L) {
  dplyr::mutate(
    dt_ohlc,
    oh_intra = .data$high / .data$open - 1,
    ol_intra = .data$low / .data$open - 1,
    oc_intra = .data$close / .data$open - 1,
    hc_intra = .data$close / .data$high - 1,
    lh_intra = .data$high / .data$low - 1,
    lc_intra = .data$close / .data$low - 1,
    oo_inter = .data$open / lag(.data$open, n = {{ inter_lag }}) - 1,
    oh_inter = .data$high / lag(.data$open, n = {{ inter_lag }}) - 1,
    ol_inter = .data$low / lag(.data$open, n = {{ inter_lag }}) - 1,
    oc_inter = .data$close / lag(.data$open, n = {{ inter_lag }}) - 1,
    ho_inter = .data$open / lag(.data$high, n = {{ inter_lag }}) - 1,
    hh_inter = .data$high / lag(.data$high, n = {{ inter_lag }}) - 1,
    hl_inter = .data$low / lag(.data$high, n = {{ inter_lag }}) - 1,
    hc_inter = .data$close / lag(.data$high, n = {{ inter_lag }}) - 1,
    lo_inter = .data$open / lag(.data$low, n = {{ inter_lag }}) - 1,
    lh_inter = .data$high / lag(.data$low, n = {{ inter_lag }}) - 1,
    ll_inter = .data$low / lag(.data$low, n = {{ inter_lag }}) - 1,
    lc_inter = .data$close / lag(.data$low, n = {{ inter_lag }}) - 1,
    co_inter = .data$open / lag(.data$close, n = {{ inter_lag }}) - 1,
    ch_inter = .data$high / lag(.data$close, n = {{ inter_lag }}) - 1,
    cl_inter = .data$low / lag(.data$close, n = {{ inter_lag }}) - 1,
    cc_inter = .data$close / lag(.data$close, n = {{ inter_lag }}) - 1
  )
}
