#' Iterative imputation
#'
#' Iterative imputation of a data set
#'
#'
#' @inheritParams impute_cols_seq
#' @param max_iter maximum number of iterations
#' @param stop_fun a stopping function (see details below) or `NULL`. If `NULL`,
#'   iterations are only stopped after `max_iter` is reached.
#' @param initial_imputation_fun This function will do the initial imputation of
#'   the missing values. If `NULL`, no initial imputation is done. Some common
#'   choices like mean imputation are implemented in the package missMethods.
#' @param ... Further arguments passed on to `initial_imputation_fun`,
#'   `stop_fun` and [parsnip::fit_xy()], [stats::predict()] used with the
#'   `model_spec_parsnip`.
#'
#' @section stop_fun: The `stop_fun` should take the arguments `ds` (the data
#'   set imputed in the current iteration), `ds_old` (the data set imputed in
#'   the last iteration) and `M` in this order. Further arguments can be passed
#'   on via `...`. The `stop_fun` must return `FALSE` to allow for a next
#'   iteration. If `stop_fun` returns not `FALSE` the iteration is stopped and
#'   the return value of `stop_fun` is returned as result of
#'   `impute_iterative()`. Therefore, this return value should normally include
#'   the imputed data set `ds` or `ds_old`.
#'
#' @return an imputed data set.
#' @export
#'
#' @examples
#' # ToDo
impute_iterative <- function(ds,
                             model_spec_parsnip = linear_reg(),
                             max_iter = 10,
                             stop_fun = NULL,
                             initial_imputation_fun = NULL,
                             cols_used_for_imputation = "only_complete",
                             cols_order = seq_len(ncol(ds)),
                             rows_used_for_imputation = "only_complete",
                             rows_order = seq_len(nrow(ds)),
                             M = is.na(ds),
                             ...) {

  # force M before initial imputation, later it will be wrong (all FALSE)
  force(M)
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
    nr_iterations <- nr_iterations + 1
  }

  ds
}
