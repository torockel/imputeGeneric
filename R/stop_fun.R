stop_ds_difference <- function(ds, ds_old, info_list, stop_eps = 1e-6, stop_p = 1,
                               stop_sum_diffs = TRUE, stop_na_rm = TRUE) {
  differences <- abs(ds - ds_old)^stop_p
  if (stop_sum_diffs) {
    difference <- sum(differences, na.rm = stop_na_rm)
  } else {
    difference <- mean(differences, na.rm = stop_na_rm)
  }
  difference <- difference^(1 / stop_p)
  if (difference < stop_eps) {
    return(structure(ds, nr_iterations = info_list$nr_iterations))
  }
  FALSE
}
