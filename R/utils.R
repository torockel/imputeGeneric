ckeck_and_set_rows_order <- function(rows_order, ds, M) {
  if (is.character(rows_order) && length(rows_order) == 1 && !(rows_order %in% rownames(ds))) {
    rows_order <- order_rows(ds, order_option = rows_order, M = M)
  }
  rows_order
}

warn_incomplete <- function(show_warning, ds) {
  if (show_warning && anyNA(ds)) {
    warning("Imputation is not complete. There are still missing values in `ds`.")
  }
}
