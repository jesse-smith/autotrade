var_select_ranger <- function(
    outcome,
    data,
    n_permutations = 100L,
    p_value = 0.05,
    ...
  ) {
  # Formula
  outcome <- rlang::as_string(rlang::ensym(outcome))
  f <- str2lang(paste0(outcome, " ~ ."))

  # Data
  data_sel <- as.data.table(data)
  # Columns
  cols_sel <- setdiff(colnames(data), outcome)
  n_cols <- length(cols_sel)
  n_diff <- n_cols

  # Remove variables with non-positive importance until convergence
  n_zeros <- ceiling(log(p_value, 0.5))
  zero_reps <- 0L
  while (zero_reps < n_zeros) {
    # Get p-values from null
    imp <- ranger::importance(ranger::ranger(
      f, data = data_sel, importance = "impurity_corrected", ...
    ))
    med_null <- median(-imp[imp <= 0], na.rm = TRUE)
    if (is.null(med_null) || is.na(med_null) || is.infinite(med_null)) med_null <- 0
    cols_sel <- names(imp)[imp > med_null]
    data_sel <- data_sel[, c(..outcome, ..cols_sel)]
    n_diff <- n_cols - length(cols_sel)
    n_cols <- length(cols_sel)
    zero_reps <- max(0L, zero_reps - n_diff + 1L)
    message("Selected: ", n_cols, "\tRemoved: ", n_diff, "\tZeros remaining: ", n_zeros - zero_reps)
  }

  # Construct null from permuted variables (Altmann 2010)
  # Compute p-values
  # Correct p-values
  # Remove variables with p >= p_value
  message("Computing null importance distribution...")
  imp_null <- numeric()
  p <- progressr::progressor(n_permutations)
  data_null <- copy(data_sel)
  for (i in seq_len(n_permutations)) {
    data_null[, c(outcome) := sample(.SD[[..outcome]])]
    imp_null <- c(imp_null, ranger::importance(ranger::ranger(
      f, data = data_null, importance = "impurity_corrected", ...
    )))
    p()
  }
  imp <- ranger::importance(ranger::ranger(
    f, data = data_sel, importance = "impurity_corrected", ...
  ))
  p_imp <- stats::p.adjust(1 - ecdf(imp_null)(imp), method = "holm")
  cols_sel <- names(imp)[p_imp < p_value]
  data_sel <- data_sel[, c(..outcome, ..cols_sel)]
  n_diff <- n_cols - length(cols_sel)
  n_cols <- length(cols_sel)
  message("Selected: ", n_cols, "\tRemoved: ", n_diff)
  data_sel
}
