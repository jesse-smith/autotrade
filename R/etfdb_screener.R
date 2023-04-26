etfdb_screener <- function(
    asset_class = NULL,
    active_passive = NULL,
    issuer = NULL,
    brand = NULL,
    structure = NULL,
    returns_1week = NULL,
    returns_1month = NULL,
    returns_ytd = NULL,
    returns_1year = NULL,
    returns_3year = NULL,
    returns_5year = NULL,
    esg_score = NULL,
    fund_flows_1week = NULL,
    fund_flows_4week = NULL,
    fund_flows_ytd = NULL,
    fund_flows_1year = NULL,
    fund_flows_3year = NULL,
    fund_flows_5year = NULL,
    standard_deviation = NULL,
    pe_ratio = NULL,
    beta = NULL,
    volatility_5day = NULL,
    volatility_20day = NULL,
    volatility_50day = NULL,
    volatility_200day = NULL,
    holdings_number = NULL,
    holdings_in_top10 = NULL,
    holdings_in_top15 = NULL,
    holdings_in_top50 = NULL,
    popular_themes = NULL
) {
  form <- etfdb_param_form()
  # Check radio button parameters
  assert_radio(asset_class, param_form)
  assert_radio(active_passive, param_form)
  # Check checkbox parameters
  assert_check_box(issuer, param_form)
  assert_check_box(brand, param_form)
  assert_check_box(structure, param_form)
  assert_check_box(popular_themes, param_form)
  # Check slider range parameters
  assert_slider_range(returns_1week, param_form)
  assert_slider_range(returns_1month, param_form)
  assert_slider_range(returns_ytd, param_form)
  assert_slider_range(returns_1year, param_form)
  assert_slider_range(returns_3year, param_form)
  assert_slider_range(returns_5year, param_form)
  assert_slider_range(esg_score, param_form)
  assert_slider_range(fund_flows_1week, param_form)
  assert_slider_range(fund_flows_4week, param_form)
  assert_slider_range(fund_flows_ytd, param_form)
  assert_slider_range(fund_flows_1year, param_form)
  assert_slider_range(fund_flows_3year, param_form)
  assert_slider_range(fund_flows_5year, param_form)
  assert_slider_range(standard_deviation, param_form)
  assert_slider_range(pe_ratio, param_form)
  assert_slider_range(beta, param_form)
  assert_slider_range(volatility_5day, param_form)
  assert_slider_range(volatility_20day, param_form)
  assert_slider_range(volatility_50day, param_form)
  assert_slider_range(volatility_200day, param_form)
  assert_slider_range(holdings_number, param_form)
  assert_slider_range(holdings_in_top10, param_form)
  assert_slider_range(holdings_in_top15, param_form)
  assert_slider_range(holdings_in_top50, param_form)
}

assert_radio <- function(param, param_form = etfdb_param_form(), param_nm = checkmate::vname(param)) {
  if (is.null(param)) return(invisible(param))
  choice_fn <- get(paste0("etfdb_param_", param), envir = rlang::pkg_env("autotrade"))
  checkmate::assert_choice(
    param,
    choices = choice_fn(param_form),
    .var.name = param_nm
  )
}

assert_check_box <- function(param, param_form = etfdb_param_form(), param_nm = checkmate::vname(param)) {
  if (is.null(param)) return(invisible(param))
  choice_fn <- get(paste0("etfdb_param_", param), envir = rlang::pkg_env("autotrade"))
  checkmate::assert_subset(
    param,
    choices = choice_fn(param_form),
    .var.name = param_nm
  )
}

assert_slider_range <- function(param, param_form = etfdb_param_form(), param_nm = checkmate::vname(param)) {
  if (is.null(param)) return(invisible(param))
  range_fn <- get(paste0("etfdb_param_", param), envir = rlang::pkg_env("autotrade"))
  range <- range_fn(param_form)
  checkmate::assert_number(
    param,
    lower = range[["start"]],
    upper = range[["end"]],
    .var.name = param_nm
  )
}

etfdb_screener_document <- function(url = "https://etfdb.com/screener/") {
  req <- httr2::request(url)
  user_agent <- paste0(
    "autotrade/", packageVersion("autotrade"), " ",
    httr2::req_user_agent(req)$options$useragent
  )
  req <- httr2::req_user_agent(req, user_agent)
  req <- httr2::req_retry(req, max_tries = 3L)
  resp <- httr2::req_perform(req)
  httr2::resp_body_html(resp)
}

etfdb_param_form <- function(screener_document = etfdb_screener_document()) {
  xml2::xml_find_first(screener_document, "//form[@name='screener']")
}

etfdb_param_asset_class <- function(param_form = etfdb_param_form()) {
  div <- xml2::xml_find_first(param_form, "//div[@id='asset-class']")
  asset_class_divs <- xml2::xml_find_all(div, "//div[@class='panel']/div[@class='collapse']")
  xml2::xml_attr(asset_class_divs, "id")
}

etfdb_param_active_passive <- function(param_form = etfdb_param_form()) {
  etfdb_param_check_box("attributes", param_form = param_form)
}

etfdb_param_issuer <- function(param_form = etfdb_param_form()) {
  etfdb_param_check_box("issuer", param_form = param_form)
}

etfdb_param_brand <- function(param_form = etfdb_param_form()) {
  etfdb_param_check_box("brand", param_form = param_form)
}

etfdb_param_structure <- function(param_form = etfdb_param_form()) {
  etfdb_param_check_box("structure", param_form = param_form)
}

etfdb_param_expense_ratio <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "expenses-dividend",
    "expense_ratio_",
    param_form = param_form
  )
}

etfdb_param_dividend_yield <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "expenses-dividend",
    "dividend_yield_",
    param_form = param_form
  )
}

etfdb_param_assets <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "liquidity-inception-date",
    "assets_",
    param_form = param_form
  )
}

etfdb_param_average_daily_volume <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "liquidity-inception-date",
    "average_volume_",
    param_form = param_form
  )
}

etfdb_param_share_price <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "liquidity-inception-date",
    "price_",
    param_form = param_form
  )
}

etfdb_param_returns_1week <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "returns",
    "one_week_return_",
    param_form = param_form
  )
}

etfdb_param_returns_1month <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "returns",
    "one_month_return_",
    param_form = param_form
  )
}

etfdb_param_returns_ytd <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "returns",
    "ytd_",
    param_form = param_form
  )
}

etfdb_param_returns_1year <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "returns",
    "fifty_two_week_",
    param_form = param_form
  )
}

etfdb_param_returns_3year <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "returns",
    "three_ytd_",
    param_form = param_form
  )
}

etfdb_param_returns_5year <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "returns",
    "five_ytd_",
    param_form = param_form
  )
}

etfdb_param_esg_score <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "esg-scores",
    "esg_quality_score_",
    param_form = param_form
  )
}

etfdb_param_fund_flows_1week <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "fund-flows",
    "one_week_ff_",
    param_form = param_form
  )
}

etfdb_param_fund_flows_4week <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "fund-flows",
    "four_week_ff_",
    param_form = param_form
  )
}

etfdb_param_fund_flows_ytd <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "fund-flows",
    "ytd_ff_",
    param_form = param_form
  )
}

etfdb_param_fund_flows_1year <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "fund-flows",
    "one_year_ff_",
    param_form = param_form
  )
}

etfdb_param_fund_flows_3year <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "fund-flows",
    "three_year_ff_",
    param_form = param_form
  )
}

etfdb_param_fund_flows_5year <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "fund-flows",
    "five_year_ff_",
    param_form = param_form
  )
}

etfdb_param_standard_deviation <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "risk-metrics",
    "standard_deviation_",
    param_form = param_form
  )
}

etfdb_param_pe_ratio <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "risk-metrics",
    "peratio_",
    param_form = param_form
  )
}

etfdb_param_beta <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "risk-metrics",
    "beta_",
    param_form = param_form
  )
}

etfdb_param_volatility_5day <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "risk-metrics",
    "five_day_volatility_",
    param_form = param_form
  )
}

etfdb_param_volatility_20day <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "risk-metrics",
    "twenty_day_volatility_",
    param_form = param_form
  )
}

etfdb_param_volatility_50day <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "risk-metrics",
    "fifty_day_volatility_",
    param_form = param_form
  )
}

etfdb_param_volatility_200day <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "risk-metrics",
    "two_hundred_day_volatility_",
    param_form = param_form
  )
}

etfdb_param_holdings_number <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "holdings",
    "number_of_holdings_",
    param_form = param_form
  )
}

etfdb_param_holdings_in_top10 <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "holdings",
    "top_ten_holdings_",
    param_form = param_form
  )
}

etfdb_param_holdings_in_top15 <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "holdings",
    "top_fifteen_holdings_",
    param_form = param_form
  )
}

etfdb_param_holdings_in_top50 <- function(param_form = etfdb_param_form()) {
  etfdb_param_slider_range(
    "holdings",
    "top_fifty_holdings_",
    param_form = param_form
  )
}

etfdb_param_popular_themes <- function(param_form = etfdb_param_form()) {
  etfdb_param_check_box("popular-themes", "theme", param_form = param_form)
}

etfdb_param_check_box <- function(div_id, input_name = div_id, param_form = etfdb_param_form()) {
  ul_xpath <- paste0(".//div[@id='", div_id, "']//ul")
  input_xpath <- paste0(".//li//label//input[@name='", input_name, "']")
  ul <- xml2::xml_find_first(param_form, ul_xpath)
  input <- xml2::xml_find_all(ul, input_xpath)
  values <- xml2::xml_attr(input, "data-screener-value")
  values[!is.na(values)]
}

etfdb_param_slider_range <- function(div_id, input_name_prefix, param_form = etfdb_param_form()) {
  div_xpath <- paste0(".//div[@id='", div_id, "']")
  input_xpath <- paste0(".//div[@class='slider-range']//input[starts-with(@name, '", input_name_prefix, "')]")
  div <- xml2::xml_find_first(param_form, div_xpath)
  input <- xml2::xml_find_all(div, input_xpath)
  values <- as.numeric(xml2::xml_attr(input, "data-screener-value"))
  names(values) <- stringr::str_remove(xml2::xml_attr(input, "name"), input_name_prefix)
  values
}
