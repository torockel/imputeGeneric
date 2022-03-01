#' Iterative imputation
#'
#' Iterative imputation of a data set
#'
#' This function impute a data set in an iterative way. Internally, either
#' [impute_supervised()] or [impute_unsupervised()] is used, depending on the
#' values of `model_spec_parsnip`, `model_fun_unsupervised` and
#' `predict_fun_unsupervised`. If you want to use a supervised inner method,
#' `model_spec_parsnip` must be specified and `model_fun_unsupervised` and
#' `predict_fun_unsupervised` must both be `NULL`. For an unsupervised inner
#' method, `model_fun_unsupervised` and `predict_fun_unsupervised` must be
#' specified and `model_spec_parsnip` must be `NULL`. Some arguments of this
#' function are only meaningful for [impute_supervised()] or
#' [impute_unsupervised()].
#'
#'
#' @inheritParams impute_supervised
#' @param model_spec_parsnip The model type used for supervised imputation (see
#'   ([impute_supervised()] for details).
#' @param model_fun_unsupervised An unsupervised model function (see
#'   [impute_unsupervised()] for details).
#' @param predict_fun_unsupervised A predict function for unsupervised
#'   imputation (see [impute_unsupervised()] for details).
#' @param max_iter Maximum number of iterations
#' @param stop_fun A stopping function (see details below) or `NULL`. If `NULL`,
#'   iterations are only stopped after `max_iter` is reached.
#' @param initial_imputation_fun This function will do the initial imputation of
#'   the missing values. If `NULL`, no initial imputation is done. Some common
#'   choices like mean imputation are implemented in the package missMethods.
#' @param update_model How often should the model for imputation be updated?
#' @param update_ds_model How often should the data set for the inner model be
#'   updated?
#' @param stop_fun_args Further arguments passed on to `stop_fun`.
#' @param model_arg Further arguments for `model_fun_unsupervised` (see
#'   [impute_unsupervised()] for details).
#' @param ... Further arguments passed on to [stats::predict()] or
#'   `predict_fun_unsupervised`.
#'
#' @section stop_fun: The `stop_fun` should take the arguments
#'   * `ds` (the data set imputed in the current iteration)
#'   * `ds_old` (the data set imputed in the last iteration)
#'   *  a list (with named elements `M`, `nr_iterations`, `max_iter`)
#'   * `stop_fun_args`
#'   * `res_stop_fun` (the return value of `stop_fun` from the last iteration.
#'      Initial value for the first iteration: `list(stop_iter = FALSE)`)
#'   in this order.
#'
#'   To allow for a next iteration, the `stop_fun` must return a list which
#'   contains the named element `stop_iter = FALSE`. The simple return
#'   `list(stop_iter = FALSE)` will allow the iteration to continue. However,
#'   the list can include more information which are handed over to `stop_fun`
#'   in the next iteration. For example, the return value
#'   `list(stop_iter = FALSE, last_eps = 0.3)` would also lead to another
#'   iteration. If `stop_fun` does not return a list or the list does not
#'   contain  `stop_iter = FALSE` the iteration is stopped and the return value
#'   of `stop_fun` is returned as result of `impute_iterative()`. Therefore,
#'   this return value should normally include the imputed data set `ds` or
#'   `ds_old`.
#'
#'   An example for a `stop_fun` is [stop_ds_difference()].
#'
#' @return an imputed data set (or a return value of `stop_fun`)
#' @export
#' @seealso
#'   * [impute_supervised()] and [impute_unsupervised()] as the workhorses for
#'     the imputation.
#'   * [stop_ds_difference()] as an example of a stop function.
#'
#' @examples
#' set.seed(123)
#' # simple example
#' ds_mis <- missMethods::delete_MCAR(
#'   data.frame(X = rnorm(20), Y = rnorm(20)), 0.2, 1
#' )
#' impute_iterative(ds_mis, max_iter = 2)
#' # using pre-imputation
#' ds_mis <- missMethods::delete_MCAR(
#'   data.frame(X = rnorm(20), Y = rnorm(20)), 0.2
#' )
#' impute_iterative(
#'   ds_mis,
#'   max_iter = 2, initial_imputation_fun = missMethods::impute_mean
#' )
#' # example using stop_ds_difference() as stop_fun
#' ds_mis <- missMethods::delete_MCAR(
#'   data.frame(X = rnorm(20), Y = rnorm(20)), 0.2
#' )
#' ds_imp <- impute_iterative(
#'   ds_mis,
#'   initial_imputation_fun = missMethods::impute_mean,
#'   stop_fun = stop_ds_difference, stop_fun_args = list(eps = 0.5)
#' )
#' attr(ds_imp, "nr_iterations")
impute_iterative <- function(ds,
                             model_spec_parsnip = linear_reg(),
                             model_fun_unsupervised = NULL,
                             predict_fun_unsupervised = NULL,
                             max_iter = 10,
                             stop_fun = NULL,
                             initial_imputation_fun = NULL,
                             cols_used_for_imputation = "only_complete",
                             cols_order = seq_len(ncol(ds)),
                             rows_used_for_imputation = "only_complete",
                             rows_order = seq_len(nrow(ds)),
                             update_model = "every_iteration",
                             update_ds_model = "every_iteration",
                             stop_fun_args = NULL,
                             M = is.na(ds),
                             model_arg = NULL,
                             warn_incomplete_imputation = TRUE,
                             ...) {

  # force M before initial imputation, later it will be wrong (all FALSE)
  force(M)
  # Initial imputation
  if (!is.null(initial_imputation_fun)) {
    ds <- initial_imputation_fun(ds)
  }

  nr_iterations <- 1
  res_stop_fun <- do_not_stop_iter()
  while (nr_iterations <= max_iter) {
    ds_old <- ds
    if (!is.null(model_spec_parsnip) &&
      is.null(model_fun_unsupervised) && is.null(predict_fun_unsupervised)) {
      ds <- impute_supervised(
        ds,
        model_spec_parsnip = model_spec_parsnip,
        cols_used_for_imputation = cols_used_for_imputation,
        cols_order = cols_order,
        rows_used_for_imputation = rows_used_for_imputation,
        rows_order = rows_order,
        update_model = update_model,
        update_ds_model = update_ds_model,
        M = M,
        warn_incomplete_imputation = FALSE, # checked only once at the end
        ...
      )
    } else if (is.null(model_spec_parsnip) &&
      !is.null(model_fun_unsupervised) &&
      !is.null(predict_fun_unsupervised)) {
      ds <- impute_unsupervised(
        ds,
        model_fun = model_fun_unsupervised,
        predict_fun = predict_fun_unsupervised,
        rows_used_for_imputation = rows_used_for_imputation,
        rows_order = rows_order,
        update_model = update_model,
        update_ds_model = update_ds_model,
        model_arg = model_arg,
        M = M, ...
      )
    } else {
      stop(
        "Either use `model_spec_parsnip` or `model_fun_unsupervised` and ",
        "`predict_fun_unsupervised`. The other(s) must be NULL."
      )
    }


    if (!is.null(stop_fun)) {
      res_stop_fun <- stop_fun(
        ds, ds_old,
        list(M = M, nr_iterations = nr_iterations, max_iter = max_iter),
        stop_fun_args, res_stop_fun
      )
      if (!(is.list(res_stop_fun) &&
            identical(res_stop_fun$stop_iter, FALSE))) {
        return(res_stop_fun)
      }
    }
    nr_iterations <- nr_iterations + 1
  }

  warn_incomplete(warn_incomplete_imputation, ds)
  structure(ds, nr_iterations = max_iter)
}
