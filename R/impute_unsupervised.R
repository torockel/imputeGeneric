#' Unsupervised imputation
#'
#' Impute a data set with an unsupervised inner method. This function is one main
#' function which can be used inside of [impute_iterative()]. If you need
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
#'   rows? Possible choices: "only_complete", "already_imputed",
#'   "all_except_i", "all_except_i_no_update", "all", "all_no_update"
#' @param rows_order Ordering of the rows for imputation. This can be a vector with
#'   indices or an `order_option` from [order_rows()].
#' @param ... Further arguments given to `model_fun` and `predict_fun`.
#'
#' @details
#' This function imputes the rows of the data set `ds` row by
#' row The imputation order of the rows can be specified by `rows_order`.
#' Furthermore, `rows_used_for_imputation` controls which rows are used for
#' the imputation. If `ds` is pre-imputed, the missing data indicator matrix
#' can be supplied via `M`.
#'
#' The inner method used to impute the data set can be defined with `model_fun`.
#' This `model_fun` must take a data set, the missing data indicator matrix `M`
#' and the index `i` of the row which should be imputed right now. It must
#' return a model `model_imp` which is given to `predict_fun` to generate
#' imputation values for the missing values in row `i`. The `model_fun` and
#' `predict_fun` can be self-written or a predefined one (see below) is used.
#'
#'
#' @return The imputed data set.
#' @seealso [model_donor()] and [predict_donor()] for a pair of predefined
#' functions for `model_fun` and `predict_fun`.
#' @export
impute_unsupervised <- function(ds,
                                model_fun, predict_fun,
                                rows_used_for_imputation = "all_no_update",
                                rows_order = seq_len(nrow(ds)),
                                M = is.na(ds), ...) {
  ds_old <- ds
  M_start <- M
  rows_order <- ckeck_and_set_rows_order(rows_order, ds, M)

  for(i in rows_order) {
    if (!any(M[i, ])){
      break
    }

    # Get row indices
    if (rows_used_for_imputation == "only_complete") {
      rows_used_imp <- !apply(M_start, 1, any)
    } else if (rows_used_for_imputation == "already_imputed") {
      rows_used_imp <- !apply(M, 1, any)
    } else if (rows_used_for_imputation %in% c("all_except_i", "all_except_i_no_update")) {
      rows_used_imp <- seq_len(nrow(ds))[-i]
    } else if (rows_used_for_imputation %in% c("all", "all_no_update")) {
      rows_used_imp <- seq_len(nrow(ds))
    } else {
      stop(paste0("'", rows_used_for_imputation, "' is not a valid option for rows_used_for_imputation"))
    }

    if (rows_used_for_imputation %in% c("only_complete", "already_imputed", "all_except_i", "all")) {
      ds_used <- ds[rows_used_imp, ]
    } else if (rows_used_for_imputation %in% c("all_except_i_no_update", "all_no_update")) {
      ds_used <- ds_old[rows_used_imp, ]
    } else {
      stop(paste0("'", rows_used_for_imputation, "' is not a valid option for rows_used_for_imputation"))
    }

    model_imp <- model_fun(ds_used, M, i, ...)
    ds[i, M[i, ]] <- predict_fun(model_imp, ds_used, M, i, ...)

    if (rows_used_for_imputation == "already_imputed") {
      M[i, M[i, ]] <- FALSE
    }
  }
  ds
}
