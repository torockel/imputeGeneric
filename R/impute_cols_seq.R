#' Sequentially imputation (columns)
#'
#' Impute columns of a data set sequentially
#'
#' @param ds the data set to be imputed, must be a data frame with column names
#' @param model_spec_parsnip The model type used for imputation. It is defined
#'   via the `parsnip` package.
#' @param cols_used_for_imputation Which columns should be used to impute other
#'   columns? Possible choices: "only_complete", "already_imputed", "all", "all_no_update"
#' @param cols_order ordering of the columns for imputation. This can be a vector with
#'   indices or an `order_option` from [order_cols()].
#' @param rows_used_for_imputation Which rows should be used to impute other
#'   rows? Possible choices: "only_complete", "partly_complete",
#'   "already_imputed", "all_except_i", "all_except_i_no_update", "all",
#'   "all_no_update"
#' @param rows_order Ordering of the rows for imputation. This can be a vector with
#'   indices or an `order_option` from [order_rows()].
#' @param M missing data indicator matrix
#' @param ... arguments passed on to [parsnip::fit_xy()] and [stats::predict()]
#'
#' @details This function imputes the columns of the data set `ds` column by
#' column. The imputation order of the column can be specified by `cols_order`.
#' Furthermore, `cols_used_for_imputation` controls which columns are used for
#' the imputation. The same options are available for the rows of `ds` via
#' `rows_order` and `rows_used_for_imputation`. If `ds` is pre-imputed, the
#' missing data indicator matrix can be supplied via `M`.
#'
#' The options "all" and "all_no_update" for `cols_used_for_imputation` and
#' "all_except_i", "all_except_i_no_update", "all", "all_no_update" for
#' `rows_used_for_imputation` are only available, if `ds` is complete.
#' If a "_no_update" option is chosen, it will override the other options.
#' Therefore, the results of e.g. `cols_used_for_imputation` = "all_no_update"
#' combined with `rows_used_for_imputation` = "all" or
#' `rows_used_for_imputation` = "all_no_update" are the same.
#'
#' @return The imputed data set.
#' @export
#'
#' @import parsnip
#' @importFrom stats predict
#'
#'
#' @examples
#' # to be done
impute_cols_seq <- function(ds,
                            model_spec_parsnip = linear_reg(),
                            cols_used_for_imputation = "only_complete",
                            cols_order = seq_len(ncol(ds)),
                            rows_used_for_imputation = "only_complete",
                            rows_order = seq_len(nrow(ds)),
                            M = is.na(ds),
                            ...) {
  # Warning: never change M_start, ds_old in this function!
  M_start <- M
  ds_old <- ds

  if (!is.data.frame(ds) || is.null(colnames(ds)))
    stop("ds must be a data frame with colnames")

  if (
    cols_used_for_imputation %in% c("all", "all_no_update") ||
    rows_used_for_imputation %in% c("all_except_i", "all_except_i_no_update", "all", "all_no_update")) {
    if (any(is.na(ds))) {
      stop("If you want to use all rows or columns for imputation, ds must be complete.")
    }
  }

  if (is.character(cols_order) && length(cols_order) == 1 && !(cols_order %in% colnames(ds))) {
    cols_order <- order_cols(ds, order_option = cols_order, M = M)
  }

  if (is.character(rows_order) && length(rows_order) == 1 && !(rows_order %in% rownames(ds))) {
    rows_order <- order_rows(ds, order_option = rows_order, M = M)
  }

  for (k in cols_order) {
    for (i in rows_order) {
      if (!M[i, k]) { # only impute, if ds[i,k] is missing
        next
      }

      # Split ds
      # Depending on used rows and columns there are better places (more efficient)
      # to split the ds and train the model

      # Get row indices
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
        rows_used_imp <- seq_len(nrow(ds))[-i]
      } else if (rows_used_for_imputation %in% c("all", "all_no_update")) {
        rows_used_imp <- seq_len(nrow(ds))
      } else {
        stop(paste0("'", rows_used_for_imputation, "' is not a valid option for rows_used_for_imputation"))
      }

      # Get column indices
      if (cols_used_for_imputation == "only_complete") {
        cols_used_imp <- !apply(M_start, 2, any)
      } else if(cols_used_for_imputation == "already_imputed") {
        cols_used_imp <- !apply(M, 2, any)
      } else if(cols_used_for_imputation %in% c("all", "all_no_update")) {
        cols_used_imp <- seq_len(ncol(ds))[-k]
      } else {
        stop(paste0("'", cols_used_for_imputation, "' is not a valid option for cols_used_for_imputation"))
      }


      # Do the split
      if (
        rows_used_for_imputation %in% c("all_except_i_no_update", "all_no_update") ||
        cols_used_for_imputation == "all_no_update") {
        ds_train <- ds_old[rows_used_imp, ]
        ds_mis <- ds_old[i, ]
      } else {
        ds_train <- ds[rows_used_imp, ]
        ds_mis <- ds[i, ]
      }


      # Train the model and predict the missing values
      model_fit <- fit_xy(
        model_spec_parsnip,
        ds_train[, cols_used_imp, drop = FALSE],
        ds_train[, k],
        ...
      )
      ds[i, k] <- predict(model_fit, ds_mis, ...)

      if (rows_used_for_imputation == "already_imputed" || cols_used_for_imputation == "already_imputed") {
        M[i, k] <- FALSE
      }
    }
  }

  ds
}
