setTBL <- function(x, rownames = NULL) {
  data.table::setDF(x, rownames = rownames)
  data.table::setattr(x, "class", class(tibble::tibble()))
  invisible(x)
}
