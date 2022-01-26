test_that("stop_ds_difference() works returns number of iterations", {
  expect_equal(
    df_XYZ_10_stop,
    stop_ds_difference(
    df_XYZ_10, df_XYZ_10,
    info_list
  )
  )
})

test_that("stop_ds_difference() stop_p works", {
  df_XYZ_10_2 <- df_XYZ_10
  df_XYZ_10_2[, 1] <-  df_XYZ_10_2[, 1] + 2
  expect_false(stop_ds_difference(
    df_XYZ_10, df_XYZ_10_2, info_list, stop_eps = 19, stop_p = 1
  ))
  expect_false(stop_ds_difference(
    df_XYZ_10, df_XYZ_10_2, info_list, stop_p = 2,
    stop_eps = sqrt(40) - sqrt(.Machine$double.eps)
    ))
  expect_equal(
    df_XYZ_10_stop,
    stop_ds_difference(
      df_XYZ_10, df_XYZ_10_2, info_list, stop_p = 2,
      stop_eps = sqrt(40) + sqrt(.Machine$double.eps)
    )
  )
})

test_that("stop_ds_difference() switch sum/mean", {
  df_XYZ_10_2 <- df_XYZ_10
  df_XYZ_10_2[2,2] <- df_XYZ_10_2[2,2] + 1

  # sum of differences is 1 >= 0.5
  expect_false(stop_ds_difference(df_XYZ_10, df_XYZ_10_2, info_list, stop_eps = 0.5))
  # but mean is < 0.5
  expect_equal(
    df_XYZ_10_stop,
    stop_ds_difference(df_XYZ_10, df_XYZ_10_2, info_list, stop_eps = 0.5, stop_sum_diffs = FALSE)
  )
})
