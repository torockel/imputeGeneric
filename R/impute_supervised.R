#' Supervised imputation
#'
#' Impute a data set with a supervised inner method. This function is one main
#' function which can be used inside of [impute_iterative()]. If you need
#' pre-imputation or iterations, directly use [impute_iterative()].
#'
#' @param ds The data set to be imputed. Must be a data frame with column names.
#' @param model_spec_parsnip The model type used for imputation. It is defined
#'   via the `parsnip` package.
#' @param cols_used_for_imputation Which columns should be used to impute other
#'   columns? Possible choices: "only_complete", "already_imputed", "all"
#' @param cols_order Ordering of the columns for imputation. This can be a vector with
#'   indices or an `order_option` from [order_cols()].
#' @param rows_used_for_imputation Which rows should be used to impute other
#'   rows? Possible choices: "only_complete", "partly_complete", "complete_in_k",
#'   "already_imputed", "all_except_i", "all"
#' @param rows_order Ordering of the rows for imputation. This can be a vector with
#'   indices or an `order_option` from [order_rows()].
#' @param update_model How often should the model for imputation be updated?
#'   Possible choices are: "everytime" (after every imputed value),
#'   "each_column" (only one update per column) and "every_iteration" (an alias
#'   for "each_column").
#' @param update_ds_model How often should the data set for the inner model be
#'   updated? Possible choices are: "everytime" (after every imputed value),
#'   "each_column" (only one update per column) and "every_iteration".
#' @param M Missing data indicator matrix
#' @param show_warning_incomplete_imputation Should a warning be given, if the
#'   returned data set still contains `NA`?
#' @param ... Arguments passed on to [stats::predict()].
#'
#' @details
#' This function imputes the columns of the data set `ds` column by
#' column. The imputation order of the columns can be specified by `cols_order`.
#' Furthermore, `cols_used_for_imputation` controls which columns are used for
#' the imputation. The same options are available for the rows of `ds` via
#' `rows_order` and `rows_used_for_imputation`. If `ds` is pre-imputed, the
#' missing data indicator matrix can be supplied via `M`.
#'
#' The inner method can be specified via `model_spec_parsnip` which should be a
#' parsnip model type like [parsnip::linear_reg()], [parsnip::rand_forest()]
#' (for a complete list see <https://www.tidymodels.org/find/parsnip>, you can
#' also build a new parsnip model and use it inside of `impute_supervised()`,
#' see <https://www.tidymodels.org/learn/develop/models> for more information
#' on building a parsnip model).
#'
#' The options "all" for `cols_used_for_imputation` and
#' "all_except_i", "all"  for `rows_used_for_imputation` should only be used,
#' if `ds` is complete or the model (`model_spec_parsnip`) can handle missing
#' data.
#'
#' The choice `update_model = "each_column"` can be much faster than
#' `update_model = "everytime"`, especially, if the data set has many
#' missing values in some columns.
#'
#' @return The imputed data set.
#' @export
#'
#' @import parsnip
#' @importFrom stats predict
#'
#' @examples
#' ds_mis <- missMethods::delete_MCAR(data.frame(X = rnorm(20), Y = rnorm(20)), 0.2, 1)
#' impute_supervised(ds_mis)
impute_supervised <- function(ds,
                            model_spec_parsnip = linear_reg(),
                            cols_used_for_imputation = "only_complete",
                            cols_order = seq_len(ncol(ds)),
                            rows_used_for_imputation = "only_complete",
                            rows_order = seq_len(nrow(ds)),
                            update_model = "each_column",
                            update_ds_model = "each_column",
                            M = is.na(ds),
                            show_warning_incomplete_imputation = TRUE,
                            ...) {
  # Warning: never change M_start, ds_old in this function!
  M_start <- M
  ds_old <- ds

  if (!is.data.frame(ds) || is.null(colnames(ds)))
    stop("ds must be a data frame with colnames")

  if (is.character(cols_order) && length(cols_order) == 1 && !(cols_order %in% colnames(ds))) {
    cols_order <- order_cols(ds, order_option = cols_order, M = M)
  }

  rows_order <- ckeck_and_set_rows_order(rows_order, ds, M)

  update_model <- match.arg(update_model, c("everytime", "each_column", "every_iteration"))
  update_model <- ifelse(update_model == "every_iteration", "each_column", update_model)
  update_ds_model <- match.arg(update_ds_model, c("everytime", "each_column", "every_iteration"))
  check_update_combinations(update_model, update_ds_model, rows_used_for_imputation)

  for (k in cols_order) {
    if(update_model == "each_column") {
      cols_used_imp <- get_col_indices(cols_used_for_imputation, M_start, M, k)
      rows_used_imp <- get_row_indices(rows_used_for_imputation, M_start, M, k, cols_used_imp)

      if (update_ds_model == "every_iteration") {
        ds_used <- ds_old
      } else if (update_ds_model %in% c("everytime", "each_column")) {
        ds_used <- ds
      } else {
        stop("Combination of update_model and update_ds_model is not implemented.")
      }

      ds_train <- ds_used[rows_used_imp, ]
      ds_mis_k <- ds_used[M_start[, k], ]

      model_fit <- fit_xy(
        model_spec_parsnip,
        ds_train[, cols_used_imp, drop = FALSE],
        ds_train[, k]
      )

      ds[M_start[, k], k] <- predict(model_fit, ds_mis_k, ...)
      M[M_start[, k], k] <- FALSE

    } else{
      for (i in rows_order) {
        if (!M[i, k]) { # only impute, if ds[i,k] is missing
          next
        }

        # Get row and column indices
        cols_used_imp <- get_col_indices(cols_used_for_imputation, M_start, M, k)
        rows_used_imp <- get_row_indices(rows_used_for_imputation, M_start, M, k, cols_used_imp, i)

        if (update_ds_model == "every_iteration") {
          ds_used <- ds_old
        } else if (update_ds_model == "everytime") {
          ds_used <- ds
        } else { # "each_column" is not implemented
          stop("Combination of update_model and update_ds_model is not implemented.")
        }

        ds_train <- ds_used[rows_used_imp, ]
        ds_mis <- ds_used[i, ]

        # Train the model and predict the missing values
        model_fit <- fit_xy(
          model_spec_parsnip,
          ds_train[, cols_used_imp, drop = FALSE],
          ds_train[, k]
        )
        ds[i, k] <- predict(model_fit, ds_mis, ...)
        M[i, k] <- FALSE
      }
    }
  }

  warn_incomplete(show_warning_incomplete_imputation, ds)
  ds
}
