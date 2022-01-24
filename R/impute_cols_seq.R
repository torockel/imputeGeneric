#' Sequentially imputation (columns)
#'
#' Impute columns of a data set sequentially
#'
#' @param ds the data set to be imputed, must be a data frame with column names
#' @param model_spec_parsnip The model type used for imputation. It is defined
#'   via the `parsnip` package.
#' @param cols_order ordering of the columns for imputation
#' @param cols_used_for_imputation Which columns should be used to impute other
#'   columns? Possible choices: "only_complete", "already_imputed", "all"
#' @param rows_order ordering of the rows for imputation
#' @param rows_used_for_imputation Which rows should be used to impute other
#'   rows? Possible choices: "only_complete", "partly_complete",
#'   "already_imputed", "all_except_i", "all_except_i_no_update", "all",
#'   "all_no_update"
#' @param M missing data indicator matrix
#'
#' @details This function imputes the columns of the data set `ds` column by
#' column. The imputation order of the column can be specified by `cols_order`.
#' Furthermore, `cols_used_for_imputation` controls which columns are used for
#' the imputation. The same options are available for the rows of `ds` via
#' `rows_order` and `rows_used_for_imputation`. If `ds` is pre-imputed, the
#' missing data indicator matrix can be supplied via `M`.
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
                            cols_order = seq_len(ncol(ds)),
                            cols_used_for_imputation = "only_complete",
                            rows_order = seq_len(nrow(ds)),
                            rows_used_for_imputation = "only_complete",
                            M = is.na(ds)) {
  # Warning: never change M_start in this function!
  M_start <- M
  ds_old <- ds

  if(!is.data.frame(ds) || is.null(colnames(ds)))
    stop("ds must be a data frame with colnames")

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
        if (cols_used_for_imputation == "only_complete") {
          rows_used_imp <- !M_start[, k]
        } else {
          stop("not implemented")
        }
      } else if (rows_used_for_imputation == "already_imputed") {
        rows_used_imp <- !M[, k]
      } else if (rows_used_for_imputation %in% c("all_except_i", "all_except_i_no_update")) {
        rows_used_imp <- seq_len(nrow(ds))[-i]
      } else if (rows_used_for_imputation %in% c("all", "all_no_update")) {
        rows_used_imp <- seq_len(nrow(ds))
      } else {
        stop(paste0("'", rows_used_for_imputation, "' is not a valid option for rows_used for imputation"))
      }

      # Get column indices
      if (cols_used_for_imputation == "only_complete") {
        cols_used_imp <- !apply(M_start, 2, any)
      } else if(cols_used_for_imputation == "already_imputed") {
        cols_used_imp <- !apply(M, 2, any)
      } else {
        stop("not implemented")
      }


      # Do the split
      if (rows_used_for_imputation %in% c("all_except_i_no_update", "all_no_update")) {
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
        ds_train[, k]
      )
      ds[i, k] <- predict(model_fit, ds_mis)

      if (rows_used_for_imputation == "already_imputed" || cols_used_for_imputation == "already_imputed") {
        M[i, k] <- FALSE
      }
    }
  }

  ds
}
