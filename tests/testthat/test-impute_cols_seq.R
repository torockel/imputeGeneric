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

test_that("wrong option for rows_used_for_imputation throws an error", {
  expect_error(
    impute_cols_seq(df_XYZ_10_mis, rows_used_for_imputation = "notAvalidOption"),
    "'notAvalidOption' is not a valid option for rows_used for imputation"
  )
})

test_that("incomplete ds with all rows throws an error", {
  expect_error(
    impute_cols_seq(df_XY_10_X_mis, rows_used_for_imputation = "all"),
    "If you want to use all rows or columns for imputation, ds must be complete"
  )
})

test_that("complete columns and rows works", {
  ds_imp_test <- df_XYZ_10_mis
  M <- is.na(df_XYZ_10_mis)
  rows_comp <- !apply(M, 1, any)
  lm_x <- lm(X ~ Z, df_XYZ_10_mis[rows_comp, ], na.action = na.fail)
  ds_imp_test$X[M[, "X"]] <- predict(lm_x, df_XYZ_10_mis[M[, "X"], ])
  lm_y <- lm(Y ~ Z, df_XYZ_10_mis[rows_comp, ], na.action = na.fail)
  ds_imp_test$Y[M[, "Y"]] <- predict(lm_y, df_XYZ_10_mis[M[, "Y"], ])

  expect_equal(
    ds_imp_test,
    impute_cols_seq(
      df_XYZ_10_mis,
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "only_complete"
    )
  )
})

test_that("complete columns and partly_complete rows work", {
  ds_imp_test <- df_XYZ_10_mis
  M <- is.na(df_XYZ_10_mis)
  lm_x <- lm(X ~ Z, df_XYZ_10_mis[!M[, "X"], ], na.action = na.fail)
  ds_imp_test$X[M[, "X"]] <- predict(lm_x, df_XYZ_10_mis[M[, "X"], ])
  lm_y <- lm(Y ~ Z, df_XYZ_10_mis[!M[, "Y"], ], na.action = na.fail)
  ds_imp_test$Y[M[, "Y"]] <- predict(lm_y, df_XYZ_10_mis[M[, "Y"], ])

  expect_equal(
    ds_imp_test,
    impute_cols_seq(
      df_XYZ_10_mis,
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "partly_complete"
    )
  )
})

test_that("complete columns and already_imputed rows work", {
  ds_imp_test <- df_XYZ_10_mis
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  ind_comp_X <- which(!M[, "X"])
  for (ind_mis in seq_along(ind_mis_X)) {
    lm_x <- lm(X ~ Z, ds_imp_test[c(ind_comp_X, ind_mis_X[seq_len(ind_mis - 1)]), ], na.action = na.fail)
    ds_imp_test$X[ind_mis_X[ind_mis]] <- predict(lm_x, ds_imp_test[ind_mis_X[ind_mis], ])
  }
  ind_mis_Y <- which(M[, "Y"])
  ind_comp_Y <- which(!M[, "Y"])
  for (ind_mis in seq_along(ind_mis_Y)) {
    lm_y <- lm(Y ~ Z, ds_imp_test[c(ind_comp_Y, ind_mis_Y[seq_len(ind_mis - 1)]), ], na.action = na.fail)
    ds_imp_test$Y[ind_mis_Y[ind_mis]] <- predict(lm_y, ds_imp_test[ind_mis_Y[ind_mis], ])
  }

  expect_equal(
    ds_imp_test,
    impute_cols_seq(
      df_XYZ_10_mis,
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "already_imputed"
    )
  )
})

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
    impute_cols_seq(
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
    impute_cols_seq(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "all_except_i",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

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
    impute_cols_seq(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "all",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

test_that("complete columns and all_no_update rows work", {
  ds_imp_test <- df_XYZ_10_mis
  M <- is.na(df_XYZ_10_mis)
  ind_mis_X <- which(M[, "X"])
  for (ind_mis in ind_mis_X) {
    lm_x <- lm(X ~ Z, df_XYZ_10, na.action = na.fail)
    ds_imp_test$X[ind_mis] <- predict(lm_x, df_XYZ_10[ind_mis, ])
  }
  ind_mis_Y <- which(M[, "Y"])
  for (ind_mis in ind_mis_Y) {
    lm_y <- lm(Y ~ Z, df_XYZ_10, na.action = na.fail)
    ds_imp_test$Y[ind_mis] <- predict(lm_y, df_XYZ_10[ind_mis, ])
  }

  expect_equal(
    ds_imp_test,
    impute_cols_seq(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "all_no_update",
      M = is.na(df_XYZ_10_mis)
    )
  )
})
