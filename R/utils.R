warn_incomplete <- function(show_warning, ds) {
  if (show_warning && any(is.na(ds))) {
    warning("Imputation is not complete. There are still missing values in `ds`.")
  }
}
