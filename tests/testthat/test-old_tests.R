test_that("complete columns and all_except_i_no_update rows work", {
  ds_imp_test <- df_XYZ_10_mis
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  for (ind_mis in ind_mis_X) {
    lm_x <- lm(X ~ Z, df_XYZ_10[seq_len(nrow(df_XYZ_10))[-ind_mis], ], na.action = na.fail)
    ds_imp_test$X[ind_mis] <- predict(lm_x, df_XYZ_10[ind_mis, ])
  }
  ind_mis_Y <- which(M[, "Y"])
  for (ind_mis in ind_mis_Y) {
    lm_y <- lm(Y ~ Z, df_XYZ_10[seq_len(nrow(df_XYZ_10))[-ind_mis], ], na.action = na.fail)
    ds_imp_test$Y[ind_mis] <- predict(lm_y, df_XYZ_10[ind_mis, ])
  }

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "all_except_i_no_update",
      M = is.na(df_XYZ_10_mis)
    )
  )
})


test_that("complete columns and all_except_i rows work", {
  ds_imp_test <- df_XYZ_10
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  for (ind_mis in ind_mis_X) {
    lm_x <- lm(X ~ Z, ds_imp_test[seq_len(nrow(ds_imp_test))[-ind_mis], ], na.action = na.fail)
    ds_imp_test$X[ind_mis] <- predict(lm_x, ds_imp_test[ind_mis, ])
  }
  ind_mis_Y <- which(M[, "Y"])
  for (ind_mis in ind_mis_Y) {
    lm_y <- lm(Y ~ Z, ds_imp_test[seq_len(nrow(ds_imp_test))[-ind_mis], ], na.action = na.fail)
    ds_imp_test$Y[ind_mis] <- predict(lm_y, ds_imp_test[ind_mis, ])
  }

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "all_except_i",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

# Tests with update model or ds every time -------------------------------------

test_that("complete columns and all rows work", {
  ds_imp_test <- df_XYZ_10
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  for (ind_mis in ind_mis_X) {
    lm_x <- lm(X ~ Z, ds_imp_test, na.action = na.fail)
    ds_imp_test$X[ind_mis] <- predict(lm_x, ds_imp_test[ind_mis, ])
  }
  ind_mis_Y <- which(M[, "Y"])
  for (ind_mis in ind_mis_Y) {
    lm_y <- lm(Y ~ Z, ds_imp_test, na.action = na.fail)
    ds_imp_test$Y[ind_mis] <- predict(lm_y, ds_imp_test[ind_mis, ])
  }

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "all",
      M = is.na(df_XYZ_10_mis)
    )
  )
})
