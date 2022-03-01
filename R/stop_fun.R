#' Compare differences between two data sets
#'
#' This function is intended to be used as `stop_fun` inside of
#' [impute_iterative()]. It compares the difference of two (numeric) data sets
#' and return `ds`, if difference is small enough (less than `stop_args$eps`).
#'
#' @param ds A numeric data set
#' @param ds_old A numeric data set
#' @param info_list `info_list` used inside of [impute_iterative()]. Only the
#'   list element `nr_iterations` is used/needed.
#' @param stop_args A list with following named components (missing elements
#'   will be replaced by default ones):
#'   * `eps` Threshold value for the difference (default = 1e-6).
#'   * `p` Exponent used for the calculation of differences similar to
#'     Minkowski distance. For `p = 1` (default) the absolute differences are
#'     used. For `p = 2` The quadratic differences are summed and the square
#'     root of this sum is compared with `stop_eps`.
#'   * `sum_diffs` Should differences be summed (default) or averaged
#'     (`sum_diffs = FALSE`)?
#'   * `na_rm` Should `NA`-values be removed (default) when calculating the
#'      sum/average? If `na_rm = FALSE` and there are `NA`s, the function
#'      returns `FALSE`.
#' @param res_stop_fun Only needed to be a valid stop function. Internally,
#'   this argument is ignored at the moment.
#'
#' @return `list(stop_iter = FALSE)`, if the difference is too big. Otherwise
#'   `ds` with number of iterations (`nr_iterations`) as attribute.
#' @export
#'
#' @examples
#' set.seed(123)
#' ds1 <- data.frame(X = rnorm(10), Y = rnorm(10))
#' ds2 <- data.frame(X = rnorm(10), Y = rnorm(10))
#' all.equal(
#'   stop_ds_difference(ds1, ds1, list(nr_iterations = 3)),
#'   structure(ds1, nr_iterations = 3)
#' )
#' stop_ds_difference(ds1, ds2, list(nr_iterations = 42))
stop_ds_difference <- function(ds, ds_old, info_list, stop_args = list(
                                 eps = 1e-6, p = 1, sum_diffs = TRUE,
                                 na_rm = TRUE
                               ), res_stop_fun = NULL) {
  stop_args <- set_defaults_for_missing(
    stop_args, list(eps = 1e-6, p = 1, sum_diffs = TRUE, na_rm = TRUE)
  )

  differences <- abs(ds - ds_old)^stop_args$p
  if (!stop_args$na_rm && anyNA(differences)) {
    return(do_not_stop_iter())
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
  do_not_stop_iter()
}
