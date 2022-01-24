impute_iterative <- function(ds,
                             model_spec_parsnip = linear_reg(),
                             max_iter = 10,
                             stop_function = NULL,
                             initial_imputation_fun = NULL,
                             cols_used_for_imputation = "only_complete",
                             cols_order = seq_len(ncol(ds)),
                             rows_used_for_imputation = "only_complete",
                             rows_order = seq_len(nrow(ds)),
                             M = is.na(ds),
                             ...) {

  # Initial imputation
  if (!is.null(initial_imputation_fun)) {
    ds <- initial_imputation_fun(ds, ...)
  }

  nr_iterations <- 1
  while(nr_iterations <= max_iter) {
    ds_old <- ds
    ds <- impute_cols_seq(
      ds, model_spec_parsnip = model_spec_parsnip,
      cols_used_for_imputation = cols_used_for_imputation,
      cols_order = cols_order,
      rows_used_for_imputation = rows_used_for_imputation,
      rows_order = rows_order,
      M = M,
      ...
    )
    if (!is.null(stop_fun)) {
      res_stop_fun <- stop_fun(ds, ds_old, M, ...)
      if (!isTRUE(all.equal(FALSE, res_stop_fun))) {
        return(res_stop_fun)
      }
    }
  }

  ds
}
