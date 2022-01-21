test_that("no data frame or no colnames throws an error", {
  expect_error(
    impute_cols_seq(c("asdf")),
    "ds must be a data frame with colnames"
  )
  df_no_name <- data.frame(X = c(2, 3))
  colnames(df_no_name) <- NULL
  expect_error(
    impute_cols_seq(df_no_name),
    "ds must be a data frame with colnames"
  )
})


test_that("complete columns and rows works", {
  ds_imp_test <- df_XYZ_10_mis
  M <- is.na(df_XYZ_10_mis)
  rows_comp <- !apply(M, 1, any)
  lm_x <- lm(X ~ Z, df_XYZ_10_mis[rows_comp, ])
  ds_imp_test$X[M[, "X"]] <- predict(lm_x, df_XYZ_10_mis[M[, "X"], ])
  lm_y <- lm(Y ~ Z, df_XYZ_10_mis[rows_comp, ])
  ds_imp_test$Y[M[, "Y"]] <- predict(lm_y, df_XYZ_10_mis[M[, "Y"], ])

  expect_equal(
    ds_imp_test,
    impute_cols_seq(
      df_XYZ_10_mis,
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "only_complete")
  )
})


