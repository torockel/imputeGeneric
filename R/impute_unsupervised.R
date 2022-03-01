#' Unsupervised imputation
#'
#' Impute a data set with an unsupervised inner method. This function is one
#' main function which can be used inside of [impute_iterative()]. If you need
#' pre-imputation or iterations, directly use [impute_iterative()].
#'
#' @inheritParams impute_supervised
#' @param model_fun An unsupervised model function which take as arguments
#'   `ds_used` (the data set used to build the model, specified via
#'   `rows_used_for_imputation`), `M` and `i` (the index of the row currently
#'   under imputation).
#' @param predict_fun A predict function which uses the via `model_fun`
#'  generated model (`model_imp`) to predict the missing values of a row. It
#'  should take the arguments `model_imp`, `ds_used`, `M` and `i`.
#' @param rows_used_for_imputation Which rows should be used to impute other
#'   rows? Possible choices:
#'   "only_complete", "already_imputed", "all_except_i", "all"
#' @param rows_order Ordering of the rows for imputation. This can be a vector
#'   with indices or an `order_option` from [order_rows()].
#' @param update_model How often should the model for imputation be updated?
#'   Possible choices are: "everytime" (after every imputed value) and
#'   "every_iteration" (only one model is created and used for all missing
#'   values).
#' @param update_ds_model How often should the data set for the inner model be
#'   updated? Possible choices are: "everytime" (after every imputed value),
#'   and "every_iteration".
#' @param model_arg Further arguments for `model_fun`. This can be a list, if
#'   it is more than one argument.
#' @param ... Further arguments given to `predict_fun`.
#'
#' @details
#' This function imputes the rows of the data set `ds` row by
#' row. The imputation order of the rows can be specified by `rows_order`.
#' Furthermore, `rows_used_for_imputation` controls which rows are used for
#' the imputation. If `ds` is pre-imputed, the missing data indicator matrix
#' can be supplied via `M`.
#'
#' The inner method used to impute the data set can be defined with `model_fun`.
#' This `model_fun` must take a data set, the missing data indicator matrix `M`,
#' the index `i` of the row which should be imputed right now (which is `NULL`,
#' if the model is updated only once per iteration or only uses complete rows)
#' and `model_arg` in this order. It must return a model `model_imp` which is
#' given to `predict_fun` to generate imputation values for the missing values
#' in a row `i`. The `model_fun` and `predict_fun` can be self-written or a
#' predefined one (see below) can be used.
#'
#' If `update_model = "every_iteration"` only one model is fitted and the
#' argument `update_ds_model` is ignored. This option can be considerably
#' faster than `update_model = "everytime"`, especially, for data sets with
#' many rows with missing values. However, some methods (like nearest
#' neighbors) need `update_model = "everytime"`.
#'
#'
#' @return The imputed data set.
#' @seealso [model_donor()] and [predict_donor()] for a pair of predefined
#' functions for `model_fun` and `predict_fun`.
#' @export
#' @examples
#' ds_mis <- missMethods::delete_MCAR(
#'   data.frame(X = rnorm(20), Y = rnorm(20)), 0.2, 1
#' )
#' impute_unsupervised(ds_mis, model_donor, predict_donor)
#' # knn imputation with k = 2
#' impute_unsupervised(ds_mis, model_donor, predict_donor,
#'   update_model = "everytime", model_arg = list(k = 2)
#' )
impute_unsupervised <- function(ds,
                                model_fun, predict_fun,
                                rows_used_for_imputation = "only_complete",
                                rows_order = seq_len(nrow(ds)),
                                update_model = "every_iteration",
                                update_ds_model = "every_iteration",
                                model_arg = NULL,
                                M = is.na(ds), ...) {
  ds_old <- ds
  M_start <- M
  rows_order <- ckeck_and_set_rows_order(rows_order, ds, M)
  stopifnot(
    "invalid choice for rows_used_for_imputation" =
      rows_used_for_imputation %in%
      c("only_complete", "already_imputed", "all_except_i", "all")
  )
  update_model <- match.arg(update_model, c("everytime", "every_iteration"))
  update_ds_model <- match.arg(
    update_ds_model, c("everytime", "every_iteration")
  )
  rows_incomplete <- which(apply(M[rows_order, ], 1, any))

  if (update_model == "every_iteration") {
    rows_used_imp <- get_row_indices(
      rows_used_for_imputation, M_start, M, cols_used_imp = seq_len(ncol(ds))
      )
    model_imp <- model_fun(ds[rows_used_imp, ], M, NULL, model_arg)
    for (i in rows_incomplete) {
      ds[i, M[i, ]] <- predict_fun(model_imp, ds, M, i, ...)
    }
  } else {
    for (i in rows_incomplete) {
      # Get row indices
      rows_used_imp <- get_row_indices(
        rows_used_for_imputation, M_start, M,
        cols_used_imp = seq_len(ncol(ds)), i = i
      )

      if (update_ds_model == "everytime") {
        ds_used <- ds
      } else {
        ds_used <- ds_old
      }
      model_imp <- model_fun(ds_used[rows_used_imp, ], M, i, model_arg)

      ds[i, M[i, ]] <- predict_fun(model_imp, ds_used, M, i, ...)
      M[i, M[i, ]] <- FALSE
    }
  }
  ds
}
