test_that("stop_ds_difference works returns number of iterations", {
  expect_equal(
    structure(df_XYZ_10, nr_iterations = 42),
    stop_ds_difference(
    df_XYZ_10, df_XYZ_10,
    list(M = is.na(df_XYZ_10_mis), nr_iterations = 42, max_iter = 70)
  )
  )
})
