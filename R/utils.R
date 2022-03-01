ckeck_and_set_rows_order <- function(rows_order, ds, M) {
  if (is.character(rows_order) && length(rows_order) == 1 &&
    !(rows_order %in% rownames(ds))) {
    rows_order <- order_rows(ds, order_option = rows_order, M = M)
  }
  rows_order
}

do_not_stop_iter <- function() {
  list(stop_iter = FALSE)
}

warn_incomplete <- function(show_warning, ds) {
  if (show_warning && anyNA(ds)) {
    warning(
      "Imputation is not complete. There are still missing values in `ds`."
    )
  }
}

get_row_indices <- function(rows_used_for_imputation, M_start = NULL, M = NULL,
                            k = NULL, cols_used_imp = NULL, i = NULL) {
  if (rows_used_for_imputation == "only_complete") {
    rows_used_imp <- !apply(M_start, 1, any)
  } else if (rows_used_for_imputation == "partly_complete") {
    rows_used_imp <- !apply(M_start[, c(cols_used_imp, k)], 1, any)
  } else if (rows_used_for_imputation == "complete_in_k") {
    rows_used_imp <- !M_start[, k]
  } else if (rows_used_for_imputation == "already_imputed") {
    rows_used_imp <- !apply(M[, c(cols_used_imp, k)], 1, any)
  } else if (rows_used_for_imputation == "all_except_i") {
    rows_used_imp <- seq_len(nrow(M))[-i]
  } else if (rows_used_for_imputation == "all") {
    rows_used_imp <- seq_len(nrow(M))
  } else {
    stop(paste0(
      "'", rows_used_for_imputation,
      "' is not a valid option for rows_used_for_imputation"
    ))
  }
  if (is.logical(rows_used_imp)) {
    rows_used_imp <- which(rows_used_imp)
  }
  rows_used_imp
}

get_col_indices <- function(cols_used_for_imputation, M_start, M, k = NULL) {
  if (cols_used_for_imputation == "only_complete") {
    cols_used_imp <- which(!apply(M_start, 2, any))
  } else if (cols_used_for_imputation == "already_imputed") {
    cols_used_imp <- which(!apply(M, 2, any))
  } else if (cols_used_for_imputation == "all") {
    cols_used_imp <- seq_len(ncol(M))[-k]
  } else {
    stop(paste0(
      "'", cols_used_for_imputation,
      "' is not a valid option for cols_used_for_imputation"
    ))
  }
  cols_used_imp
}

check_update_combinations <- function(update_model, update_ds_model,
                                      rows_used_for_imputation) {
  if (update_model == "each_column" &&
    rows_used_for_imputation == "all_except_i") {
    warning(paste(
      "update_model is set to everytime because a new model is constructed",
      "for every row"
    ))
    assign("update_model", "everytime", pos = parent.frame(1))
  }
}

set_defaults_for_missing <- function(arg_list, default_args) {
  if (is.null(arg_list)) {
    return(default_args)
  }
  arg_names <- names(default_args)
  for (arg_name in arg_names) {
    if (is.null(arg_list[[arg_name]])) {
      arg_list[[arg_name]] <- default_args[[arg_name]]
    }
  }
  arg_list
}
