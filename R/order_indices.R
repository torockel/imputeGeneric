#' Order row or column indices
#'
#' @inheritParams order_rows
#' @param dimension Should be "rows" or "cols" (is not checked, everything
#'   except "cols" will return the value for "rows").
#'
#' @return The ordered indices
#' @noRd
order_indices <- function(ds, order_option, dimension, M = is.na(ds)) {
  if (dimension == "cols") {
    M <- t(M)
  }
  rows_order <- switch(order_option,
    lowest_md_first = order(rowSums(M), decreasing = FALSE),
    highest_md_first = order(rowSums(M), decreasing = TRUE),
    increasing_index = seq_len(nrow(M)),
    decreasing_index = seq(nrow(M), 1, by = -1),
    "not_implemented"
  )
  if (isTRUE(all.equal(rows_order, "not_implemented"))) {
    stop(paste0("`", order_option, "` is not a valid option for ordering."))
  }
  rows_order
}


#' Order row indices
#'
#' Order the indices of the rows of `ds` for imputation.
#'
#' @param ds A data frame
#' @param order_option This option defines the ordering of the indices. Possible
#'   choices are "lowest_md_first", "highest_md_first", "increasing_index",
#'   "decreasing_index".
#' @param M Missing data indicator matrix
#'
#' @return The ordered row indices of `ds` as a vector.
#' @export
#'
#' @examples
#' ds <- data.frame(X = c(NA, NA, 3, 4), Y = c(1, NA, NA, 4))
#' order_rows(ds, "lowest_md_first")
order_rows <- function(ds, order_option, M = is.na(ds)) {
  order_indices(ds, order_option, "rows", M)
}


#' Order column indices
#'
#' Order the indices of the columns of `ds` for imputation.
#'
#' @inheritParams order_rows
#'
#' @return The ordered column indices of `ds` as a vector.
#' @export
#'
#' @examples
#' ds <- data.frame(X = c(NA, NA, NA, 4), Y = rep(2, 4), Z = c(1, NA, NA, 4))
#' order_cols(ds, "highest_md_first")
order_cols <- function(ds, order_option, M = is.na(ds)) {
  order_indices(ds, order_option, "cols", M)
}
