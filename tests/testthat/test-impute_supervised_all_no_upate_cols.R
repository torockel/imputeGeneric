test_that("all_no_update columns and (complete and partly_complete) rows works", {
  ds_imp_test <- df_XYZ_10
  M <- is.na(df_XYZ_10_mis)
  rows_comp <- !apply(M, 1, any)
  lm_x <- lm(X ~ Z + Y, df_XYZ_10[rows_comp, ], na.action = na.fail)
  ds_imp_test$X[M[, "X"]] <- predict(lm_x, df_XYZ_10[M[, "X"], ])
  lm_y <- lm(Y ~ Z + X, df_XYZ_10[rows_comp, ], na.action = na.fail)
  ds_imp_test$Y[M[, "Y"]] <- predict(lm_y, df_XYZ_10[M[, "Y"], ])

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all_no_update",
      rows_used_for_imputation = "only_complete",
      M = is.na(df_XYZ_10_mis)
    )
  )

  # if all columns are considered, partly_complete are only complete observed rows
  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all_no_update",
      rows_used_for_imputation = "partly_complete",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

test_that("all_no_update columns and already_imputed rows work", {
  # not a really sensible combination...
  ds_imp_test <- df_XYZ_10
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  ind_mis_Y <- which(M[, "Y"])
  rows_comp <- !apply(M, 1, any)
  for (ind_mis in ind_mis_X) {
    lm_x <- lm(X ~ Z + Y, df_XYZ_10[rows_comp, ], na.action = na.fail)
    ds_imp_test$X[ind_mis] <- predict(lm_x, df_XYZ_10[ind_mis, ])
    M[ind_mis, "X"] <- FALSE
    rows_comp <- !apply(M, 1, any)
  }
  for (ind_mis in ind_mis_Y) {
    lm_y <- lm(Y ~ Z + X, df_XYZ_10[rows_comp, ], na.action = na.fail)
    ds_imp_test$Y[ind_mis] <- predict(lm_y, df_XYZ_10[ind_mis, ])
    M[ind_mis, "Y"] <- FALSE
    rows_comp <- !apply(M, 1, any)
  }

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all_no_update",
      rows_used_for_imputation = "already_imputed",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

test_that("all_no_update columns and (all_except_i and all_except_i_no_update) rows work", {
  ds_imp_test <- df_XYZ_10_mis
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  for (ind_mis in ind_mis_X) {
    lm_x <- lm(X ~ Z + Y, df_XYZ_10[seq_len(nrow(df_XYZ_10))[-ind_mis], ], na.action = na.fail)
    ds_imp_test$X[ind_mis] <- predict(lm_x, df_XYZ_10[ind_mis, ])
  }
  ind_mis_Y <- which(M[, "Y"])
  for (ind_mis in ind_mis_Y) {
    lm_y <- lm(Y ~ Z + X, df_XYZ_10[seq_len(nrow(df_XYZ_10))[-ind_mis], ], na.action = na.fail)
    ds_imp_test$Y[ind_mis] <- predict(lm_y, df_XYZ_10[ind_mis, ])
  }

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all_no_update",
      rows_used_for_imputation = "all_except_i_no_update",
      M = is.na(df_XYZ_10_mis)
    )
  )

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all_no_update",
      rows_used_for_imputation = "all_except_i",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

test_that("all_no_update columns and (all and all_no_update) rows work", {
  ds_imp_test <- df_XYZ_10
  M <- is.na(df_XYZ_10_mis)
  lm_x <- lm(X ~ Z + Y, df_XYZ_10, na.action = na.fail)
  ind_mis_X <- which(M[, "X"])
  ds_imp_test$X[ind_mis_X] <- predict(lm_x, df_XYZ_10[ind_mis_X, ])
  ind_mis_Y <- which(M[, "Y"])
  lm_y <- lm(Y ~ Z + X, df_XYZ_10, na.action = na.fail)
  ds_imp_test$Y[ind_mis_Y] <- predict(lm_y, df_XYZ_10[ind_mis_Y, ])

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all_no_update",
      rows_used_for_imputation = "all",
      M = is.na(df_XYZ_10_mis)
    )
  )

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all_no_update",
      rows_used_for_imputation = "all_no_update",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

