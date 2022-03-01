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
    df_XYZ_10_mis,
    max_iter = 2,
    rows_used_for_imputation = "all", cols_used_for_imputation = "all"
  ))
})

test_that("initial_imputation_fun and max_iter = 1 works", {
  ds_imp <- missMethods::impute_mean(df_XYZ_10_mis)
  ds_imp <- impute_supervised(
    ds_imp,
    cols_used_for_imputation = "all",
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
  for (i in 1:2) {
    ds_imp <- impute_supervised(
      ds_imp,
      M = is.na(df_XYZ_10_mis),
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

test_that("supervised and unsupervised at once throwns an error", {
  expect_error(
    impute_iterative(
      df_XYZ_10_mis,
      model_spec_parsnip = linear_reg(),
      model_fun_unsupervised = model_donor,
      predict_fun_unsupervised = predict_donor
    ),
    "Either use `model_spec_parsnip` or `model_fun_unsupervised` and"
  )
})

test_that("unsupervised imputation works", {
  ds_imp <- impute_iterative(
    df_XYZ_10_mis,
    model_spec_parsnip = NULL,
    model_fun_unsupervised = model_donor,
    predict_fun_unsupervised = predict_donor
  )
  expect_false(anyNA(ds_imp))
  expect_true(all(ds_imp$X %in% na.omit(df_XYZ_10_mis$X)))
  expect_true(all(ds_imp$Y %in% na.omit(df_XYZ_10_mis$Y)))
})

test_that("impute_iterative() works out of the box,
          if there are enough complete rows/cols", {
  set.seed(123)
  n <- 50
  ds_mis <- data.frame(X = rnorm(n), Y = rnorm(n), Z = rnorm(n))
  ds_mis$Z[sample.int(n, 15)] <- NA
  ds_mis$Y[sample.int(n, 10)] <- NA
  # impute data set
  ds_imp3 <- impute_iterative(ds_mis, max_iter = 1)
  expect_false(anyNA(ds_imp3))
})

test_that("impute_iterative() works with stop_fun", {
  ds_imp <- impute_iterative(
    df_XYZ_10_mis,
    initial_imputation_fun = missMethods::impute_mean,
    stop_fun = stop_ds_difference,
    cols_used_for_imputation = "all",
    rows_used_for_imputation = "all",
    stop_fun_args = list(eps = 0.4)
  )
  expect_true(is.data.frame(ds_imp))
  expect_equal(dim(ds_imp), c(10, 3))
  expect_equal(attr(ds_imp, "nr_iterations"), 3)
})
