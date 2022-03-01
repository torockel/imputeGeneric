test_that("impute_unsupervised() error for wrong rows_used_for_imputation", {
  expect_error(
    impute_unsupervised(
      df_XYZ_10_mis, model_donor, predict_donor,
      rows_used_for_imputation = "asdfa"
    ),
    "invalid choice for rows_used_for_imputation"
  )
})

test_that("impute_unsupervised() works for update_model = every_iteration", {
  ds_imp <- impute_unsupervised(
    df_XYZ_10_mis, model_donor, predict_donor,
    update_model = "every_iteration"
  )
  expect_false(anyNA(ds_imp))
  expect_true(is.data.frame(ds_imp))
  expect_equal(dim(ds_imp), c(10, 3))
})

test_that("impute_unsupervised() works for update_model = everytime", {
  ds_imp <- impute_unsupervised(
    df_XYZ_10_mis, model_donor, predict_donor,
    update_model = "everytime",
    rows_used_for_imputation = "all"
  )
  expect_false(anyNA(ds_imp))
  expect_true(is.data.frame(ds_imp))
  expect_equal(dim(ds_imp), c(10, 3))
})


test_that("impute_unsupervised() works for update model and ds everytime", {
  ds_imp <- impute_unsupervised(
    df_XYZ_10_mis, model_donor, predict_donor,
    update_model = "everytime",
    update_ds_model = "everytime",
    rows_used_for_imputation = "all"
  )
  expect_false(anyNA(ds_imp))
  expect_true(is.data.frame(ds_imp))
  expect_equal(dim(ds_imp), c(10, 3))
})
