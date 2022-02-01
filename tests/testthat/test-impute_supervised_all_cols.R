test_that("all columns and (complete and partly_complete) rows works", {
  ds_imp_test <- df_XYZ_10
  M <- is.na(df_XYZ_10_mis)
  rows_comp <- !apply(M, 1, any)
  lm_x <- lm(X ~ Z + Y, df_XYZ_10[rows_comp, ], na.action = na.fail)
  ds_imp_test$X[M[, "X"]] <- predict(lm_x, df_XYZ_10[M[, "X"], ])
  lm_y <- lm(Y ~ Z + X, ds_imp_test[rows_comp, ], na.action = na.fail)
  ds_imp_test$Y[M[, "Y"]] <- predict(lm_y, ds_imp_test[M[, "Y"], ])

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "only_complete",
      M = is.na(df_XYZ_10_mis)
    )
  )
  # if all columns are considered, partly_complete are only complete observed rows
  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "partly_complete",
      M = is.na(df_XYZ_10_mis)
    )
  )

})

test_that("all columns and already_imputed rows work", {
  ds_imp_test <- df_XYZ_10
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  ind_mis_Y <- which(M[, "Y"])
  rows_comp <- !apply(M, 1, any)
  for (ind_mis in ind_mis_X) {
    lm_x <- lm(X ~ Z + Y, ds_imp_test[rows_comp, ], na.action = na.fail)
    ds_imp_test$X[ind_mis] <- predict(lm_x, ds_imp_test[ind_mis, ])
    M[ind_mis, "X"] <- FALSE
    rows_comp <- !apply(M, 1, any)
  }
  for (ind_mis in ind_mis_Y) {
    lm_y <- lm(Y ~ Z + X, ds_imp_test[rows_comp, ], na.action = na.fail)
    ds_imp_test$Y[ind_mis] <- predict(lm_y, ds_imp_test[ind_mis, ])
    M[ind_mis, "Y"] <- FALSE
    rows_comp <- !apply(M, 1, any)
  }

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "already_imputed",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

test_that("all columns and all_except_i_no_update rows work", {
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
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "all_except_i_no_update",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

test_that("all columns and all_except_i rows work", {
  ds_imp_test <- df_XYZ_10
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  for (ind_mis in ind_mis_X) {
    lm_x <- lm(X ~ Z + Y, ds_imp_test[seq_len(nrow(ds_imp_test))[-ind_mis], ], na.action = na.fail)
    ds_imp_test$X[ind_mis] <- predict(lm_x, ds_imp_test[ind_mis, ])
  }
  ind_mis_Y <- which(M[, "Y"])
  for (ind_mis in ind_mis_Y) {
    lm_y <- lm(Y ~ Z + X, ds_imp_test[seq_len(nrow(ds_imp_test))[-ind_mis], ], na.action = na.fail)
    ds_imp_test$Y[ind_mis] <- predict(lm_y, ds_imp_test[ind_mis, ])
  }

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "all_except_i",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

test_that("all columns and all rows work", {
  ds_imp_test <- df_XYZ_10
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  for (ind_mis in ind_mis_X) {
    lm_x <- lm(X ~ Z + Y, ds_imp_test, na.action = na.fail)
    ds_imp_test$X[ind_mis] <- predict(lm_x, ds_imp_test[ind_mis, ])
  }
  ind_mis_Y <- which(M[, "Y"])
  for (ind_mis in ind_mis_Y) {
    lm_y <- lm(Y ~ Z + X, ds_imp_test, na.action = na.fail)
    ds_imp_test$Y[ind_mis] <- predict(lm_y, ds_imp_test[ind_mis, ])
  }

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "all",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

test_that("all columns and rows with missing value and rpart works", {
  ds_imp_test <- df_XYZ_10_mis
  rpart_x <- fit(decision_tree("regression"), X ~ Y + Z, df_XYZ_10_mis)
  ds_imp_test$X[is.na(df_XYZ_10_mis$X)] <- unlist(predict(rpart_x, df_XYZ_10_mis[is.na(df_XYZ_10_mis$X), ]))
  rpart_y <- fit(decision_tree("regression"), Y ~ X + Z, df_XYZ_10_mis)
  ds_imp_test$Y[is.na(df_XYZ_10_mis$Y)] <- unlist(predict(rpart_y, df_XYZ_10_mis[is.na(df_XYZ_10_mis$Y), ]))
  expect_false(any(is.na(ds_imp_test)))
  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10_mis, decision_tree("regression"),
      cols_used_for_imputation = "all_no_update", rows_used_for_imputation = "all_no_update"
      )
  )
})

test_that("all columns and all_no_update rows work", {
  ds_imp_test <- df_XYZ_10_mis
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  for (ind_mis in ind_mis_X) {
    lm_x <- lm(X ~ Z + Y, df_XYZ_10, na.action = na.fail)
    ds_imp_test$X[ind_mis] <- predict(lm_x, df_XYZ_10[ind_mis, ])
  }
  ind_mis_Y <- which(M[, "Y"])
  for (ind_mis in ind_mis_Y) {
    lm_y <- lm(Y ~ Z + X, df_XYZ_10, na.action = na.fail)
    ds_imp_test$Y[ind_mis] <- predict(lm_y, df_XYZ_10[ind_mis, ])
  }

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "all_no_update",
      M = is.na(df_XYZ_10_mis)
    )
  )
})
