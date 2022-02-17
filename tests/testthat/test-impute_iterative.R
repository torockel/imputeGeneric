test_that("initial_imputation_fun works", {
  expect_equal(
    structure(missMethods::impute_mean(df_XYZ_10_mis), nr_iterations = 0),
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
  ds_imp <- impute_supervised(
    ds_imp, cols_used_for_imputation = "all",
    rows_used_for_imputation = "all", M = is.na(df_XYZ_10_mis),
    update_ds_model = "every_iteration"
    )
  ds_imp <- structure(ds_imp, nr_iterations = 1)
  expect_equal(
    ds_imp,
    impute_iterative(
      df_XYZ_10_mis,
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "all",
      update_ds_model = "every_iteration",
      initial_imputation_fun = missMethods::impute_mean, max_iter = 1
    )
  )
})

test_that("initial_imputation_fun and max_iter = 2 works", {
  ds_imp <- missMethods::impute_mean(df_XYZ_10_mis)
  for(i in 1:2) {
    ds_imp <- impute_supervised(
      ds_imp, M = is.na(df_XYZ_10_mis),
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "all",
      update_ds_model = "every_iteration"
    )
  }
  ds_imp <- structure(ds_imp, nr_iterations = 2)
  expect_equal(
    ds_imp,
    impute_iterative(
      df_XYZ_10_mis,
      initial_imputation_fun = missMethods::impute_mean, max_iter = 2,
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "all",
      update_ds_model = "every_iteration"
    )
  )
})
