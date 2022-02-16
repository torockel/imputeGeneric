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

get_row_indices <- function(rows_used_for_imputation, cols_used_for_imputation, M_start, M, i, k) {
  if (rows_used_for_imputation == "only_complete") {
    rows_used_imp <- !apply(M_start, 1, any)
  } else if (rows_used_for_imputation  == "partly_complete"){
    if (cols_used_for_imputation %in% c("all", "all_no_update")) {
      rows_used_imp <- !apply(M_start, 1, any)
    } else {
      rows_used_imp <- !M_start[, k]
    }
  } else if (rows_used_for_imputation == "already_imputed") {
    if (cols_used_for_imputation %in% c("all", "all_no_update")) {
      rows_used_imp <- !apply(M, 1, any)
    } else {
      rows_used_imp <- !M[, k]
    }
  } else if (rows_used_for_imputation %in% c("all_except_i", "all_except_i_no_update")) {
    rows_used_imp <- seq_len(nrow(M))[-i]
  } else if (rows_used_for_imputation %in% c("all", "all_no_update")) {
    rows_used_imp <- seq_len(nrow(M))
  } else {
    stop(paste0("'", rows_used_for_imputation, "' is not a valid option for rows_used_for_imputation"))
  }
  rows_used_imp
}

get_col_indices <- function(cols_used_for_imputation, M_start, M, k) {
  if (cols_used_for_imputation == "only_complete") {
    cols_used_imp <- !apply(M_start, 2, any)
  } else if(cols_used_for_imputation == "already_imputed") {
    cols_used_imp <- !apply(M, 2, any)
  } else if(cols_used_for_imputation %in% c("all", "all_no_update")) {
    cols_used_imp <- seq_len(ncol(M))[-k]
  } else {
    stop(paste0("'", cols_used_for_imputation, "' is not a valid option for cols_used_for_imputation"))
  }
  cols_used_imp
}
