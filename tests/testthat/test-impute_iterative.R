test_that("initial_imputation_fun works", {
  expect_equal(
    missMethods::impute_mean(df_XYZ_10_mis),
    impute_iterative(
      df_XYZ_10_mis,
      initial_imputation_fun = missMethods::impute_mean, max_iter = 0
      )
  )
})

test_that("Warning for incomplete ds is shown (only once)", {
  expect_snapshot(impute_iterative(
    df_XYZ_10_mis, max_iter = 2, rows_used_for_imputation =  "all", cols_used_for_imputation = "all"))
})

test_that("initial_imputation_fun and max_iter = 1 works", {
  ds_imp <- missMethods::impute_mean(df_XYZ_10_mis)
  ds_imp <- impute_cols_seq(ds_imp, M = is.na(df_XYZ_10_mis))
  expect_equal(
    ds_imp,
    impute_iterative(
      df_XYZ_10_mis,
      initial_imputation_fun = missMethods::impute_mean, max_iter = 1
    )
  )
})

test_that("initial_imputation_fun and max_iter = 2 works", {
  ds_imp <- missMethods::impute_mean(df_XYZ_10_mis)
  for(i in 1:2) {
    ds_imp <- impute_cols_seq(ds_imp, M = is.na(df_XYZ_10_mis), rows_used_for_imputation = "all_no_update")
  }
  expect_equal(
    ds_imp,
    impute_iterative(
      df_XYZ_10_mis,
      initial_imputation_fun = missMethods::impute_mean, max_iter = 2,
      rows_used_for_imputation = "all_no_update"
    )
  )
})

