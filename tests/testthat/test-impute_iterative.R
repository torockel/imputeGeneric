test_that("initial_imputation_fun works", {
  expect_equal(
    missMethods::impute_mean(df_XYZ_10_mis),
    impute_iterative(
      df_XYZ_10_mis,
      initial_imputation_fun = missMethods::impute_mean, max_iter = 0
      )
  )
})
