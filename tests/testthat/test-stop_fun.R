test_that("stop_ds_difference() works returns number of iterations", {
  expect_equal(
    df_XYZ_10_stop,
    stop_ds_difference(
      df_XYZ_10, df_XYZ_10,
      info_list
    )
  )
})

test_that("stop_ds_difference() stop_args$p works", {
  df_XYZ_10_2 <- df_XYZ_10
  df_XYZ_10_2[, 1] <- df_XYZ_10_2[, 1] + 2
  expect_equal(
    stop_ds_difference(
      df_XYZ_10, df_XYZ_10_2, info_list,
      list(eps = 19, p = 1, na_rm = TRUE, sum_diffs = TRUE)
    ),
    do_not_stop_iter()
  )
  expect_equal(
    stop_ds_difference(
      df_XYZ_10, df_XYZ_10_2, info_list,
      list(
        p = 2, eps = sqrt(40) - sqrt(.Machine$double.eps), na_rm = TRUE,
        sum_diffs = TRUE
      )
    ),
    do_not_stop_iter()
  )
  expect_equal(
    df_XYZ_10_stop,
    stop_ds_difference(
      df_XYZ_10, df_XYZ_10_2, info_list,
      list(
        p = 2, eps = sqrt(40) + sqrt(.Machine$double.eps), na_rm = TRUE,
        sum_diffs = TRUE
      )
    )
  )
})

test_that("stop_ds_difference() switch sum/mean", {
  df_XYZ_10_2 <- df_XYZ_10
  df_XYZ_10_2[2, 2] <- df_XYZ_10_2[2, 2] + 1

  # sum of differences is 1 >= 0.5
  expect_equal(
    stop_ds_difference(
      df_XYZ_10, df_XYZ_10_2, info_list,
      list(eps = 0.5, p = 1, sum_diffs = TRUE, na_rm = TRUE)
    ),
    do_not_stop_iter()
  )
  # but mean is < 0.5
  expect_equal(
    df_XYZ_10_stop,
    stop_ds_difference(
      df_XYZ_10, df_XYZ_10_2, info_list,
      list(eps = 0.5, sum_diffs = FALSE, p = 1, na_rm = TRUE)
    )
  )
})

test_that("stop_ds_difference() stop_args$na_rm works", {
  expect_equal(
    df_XYZ_10_stop,
    stop_ds_difference(df_XYZ_10, df_XYZ_10_mis, info_list)
  )
  expect_equal(
    stop_ds_difference(
      df_XYZ_10, df_XYZ_10_mis, info_list,
      list(eps = 1e-6, p = 1, sum_diffs = TRUE, na_rm = FALSE)
    ),
    do_not_stop_iter()
  )
})

test_that("stop_ds_difference() works inside impute_iterative()", {
  ds_imp <- impute_supervised(
    df_XYZ_10_mis,
    cols_used_for_imputation = "only_complete",
    rows_used_for_imputation = "only_complete"
  )
  expect_equal(
    structure(ds_imp, nr_iterations = 1),
    impute_iterative(
      df_XYZ_10_mis,
      stop_fun = stop_ds_difference,
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "only_complete"
    )
  )
})
