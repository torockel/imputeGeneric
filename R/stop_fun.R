#' Compare differences between two data sets
#'
#' This function is intended to be used as `stop_fun` inside of
#' [impute_iterative()]. It compares the difference of two (numeric) data sets
#' and return `ds`, if difference is small enough (less then `stop_args$eps`).
#'
#' @param ds a numeric data set
#' @param ds_old a numeric data set
#' @param info_list info_list used inside of [impute_iterative()]. Only the list
#'   element `nr_iterations` is used/needed.
#' @param stop_args A list with following named components:
#'   * `eps` Threshold value for the difference.
#'   * `p` exponent used for the calculation of differences similar to
#'     Minkowski distance. For `p = 1` the absolute differences are used. For
#'     `p = 2` the quadratic differences are summed and the square root of
#'     this sum is compared with `stop_eps`.
#'   * `sum_diffs` Should differences be summed or averaged (`sum_diffs = FALSE`)?
#'   * `na_rm` Should `NA`-values be removed when calculating the sum/average?
#'
#' @return `FALSE`, if difference is too big. Otherwise `ds` with number of
#'   iterations (`nr_iterations`) as attribute.
#' @export
#'
#' @examples
#' # See impute_iterative()
stop_ds_difference <- function(ds, ds_old, info_list, stop_args = list(
  eps = 1e-6, p = 1, sum_diffs = TRUE, na_rm = TRUE
  )) {
  if (is.null(stop_args)) {
    stop_args <- list(eps = 1e-6, p = 1, sum_diffs = TRUE, na_rm = TRUE)
  }
  differences <- abs(ds - ds_old)^stop_args$p
  if (!stop_args$na_rm && anyNA(differences)) {
    stop("You need stop_args$na_rm = TRUE, if ds or ds_old contains missing values.")
  }
  if (stop_args$sum_diffs) {
    difference <- sum(differences, na.rm = stop_args$na_rm)
  } else {
    difference <- mean(unlist(differences), na.rm = stop_args$na_rm)
  }
  difference <- difference^(1 / stop_args$p)
  if (difference < stop_args$eps) {
    return(structure(ds, nr_iterations = info_list$nr_iterations))
  }
  FALSE
}
