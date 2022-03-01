# Warnings and errors ---------------------------------------------------------
test_that("no data frame or no colnames throws an error", {
  expect_error(
    impute_supervised(c("asdf")),
    "ds must be a data frame with colnames"
  )
  df_no_name <- data.frame(X = c(2, 3))
  colnames(df_no_name) <- NULL
  expect_error(
    impute_supervised(df_no_name),
    "ds must be a data frame with colnames"
  )
})

test_that("incomplete ds after imputation gives a warning", {
  expect_warning(
    impute_supervised(
      df_XYZ_10_mis,
      rows_used_for_imputation = "all",
      cols_used_for_imputation = "all"
    ),
    "Imputation is not complete. There are still missing values in `ds`."
  )
})

test_that("wrong option for rows_used_for_imputation throws an error", {
  expect_error(
    impute_supervised(df_XYZ_10_mis, rows_used_for_imputation = "notValidOpt"),
    "'notValidOpt' is not a valid option for rows_used_for_imputation"
  )
})

test_that("wrong option for cols_used_for_imputation throws an error", {
  expect_error(
    impute_supervised(df_XYZ_10_mis, cols_used_for_imputation = "notValidOpt"),
    "'notValidOpt' is not a valid option for cols_used_for_imputation"
  )
})

test_that("check_update_combinations() is called by impute_supervised()", {
  expect_warning(
    impute_supervised(
      df_XYZ_10,
      rows_used_for_imputation = "all_except_i",
      update_model = "each_column"
    ),
    "update_model is set to everytime because a new model is constructed"
  )
})

# Test function calls for ordering rows and cols ------------------------------
test_that("cols_order is passed to order_cols()", {
  ds_imp_highest_md_first <- impute_supervised(
    df_XYZ_10,
    cols_order = c("Y", "X", "Z"),
    cols_used_for_imputation = "already_imputed",
    M = is.na(df_XYZ_10_mis)
  )
  expect_equal(
    impute_supervised(
      df_XYZ_10,
      cols_order = "highest_md_first",
      cols_used_for_imputation = "already_imputed",
      M = is.na(df_XYZ_10_mis)
    ),
    ds_imp_highest_md_first
  )
  expect_false(isTRUE(all.equal(
    ds_imp_highest_md_first,
    impute_supervised(
      df_XYZ_10,
      cols_order = "lowest_md_first",
      cols_used_for_imputation = "already_imputed",
      M = is.na(df_XYZ_10_mis)
    )
  )))
})

test_that("rows_order and M is passed to order_rows()", {
  expect_equal(
    impute_supervised(
      df_XYZ_10,
      rows_order = "highest_md_first",
      rows_used_for_imputation = "all",
      cols_used_for_imputation = "already_imputed",
      M = is.na(df_XYZ_10_mis)
    ),
    impute_supervised(
      df_XYZ_10,
      rows_order = c(7, 1, 2, 4),
      rows_used_for_imputation = "all",
      cols_used_for_imputation = "already_imputed",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

# parsnip model via rpart -----------------------------------------------------
test_that("all columns and rows with missing value and rpart works", {
  ds_imp_test <- df_XYZ_10_mis
  rpart_x <- fit(decision_tree("regression"), X ~ Y + Z, df_XYZ_10_mis)
  ds_imp_test$X[is.na(df_XYZ_10_mis$X)] <- unlist(predict(
    rpart_x, df_XYZ_10_mis[is.na(df_XYZ_10_mis$X), ]
  ))
  rpart_y <- fit(decision_tree("regression"), Y ~ X + Z, df_XYZ_10_mis)
  ds_imp_test$Y[is.na(df_XYZ_10_mis$Y)] <- unlist(predict(
    rpart_y, df_XYZ_10_mis[is.na(df_XYZ_10_mis$Y), ]
  ))
  expect_false(anyNA(ds_imp_test))
  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10_mis, decision_tree("regression"),
      cols_used_for_imputation = "all", rows_used_for_imputation = "all",
      update_ds_model = "every_iteration"
    )
  )
})

# Basic fests for update model and model ds every column ----------------------
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
    impute_supervised(
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
    impute_supervised(
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
    lm_x <- lm(
      X ~ Z, ds_imp_test[c(ind_comp_X, ind_mis_X[seq_len(ind_mis - 1)]), ],
      na.action = na.fail
    )
    ds_imp_test$X[ind_mis_X[ind_mis]] <- predict(
      lm_x, ds_imp_test[ind_mis_X[ind_mis], ]
    )
  }
  ind_mis_Y <- which(M[, "Y"])
  ind_comp_Y <- which(!M[, "Y"])
  for (ind_mis in seq_along(ind_mis_Y)) {
    lm_y <- lm(
      Y ~ Z, ds_imp_test[c(ind_comp_Y, ind_mis_Y[seq_len(ind_mis - 1)]), ],
      na.action = na.fail
    )
    ds_imp_test$Y[ind_mis_Y[ind_mis]] <- predict(
      lm_y, ds_imp_test[ind_mis_Y[ind_mis], ]
    )
  }

  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10_mis,
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "already_imputed"
    )
  )

  expect_false(isTRUE(all.equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10_mis,
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "only_complete"
    )
  )))
})

test_that("complete columns and all rows work", {
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
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "only_complete",
      rows_used_for_imputation = "all",
      M = is.na(df_XYZ_10_mis)
    )
  )
})


# Tests for update model every column and model ds every_iteration  -----------
test_that("update model every column and model ds every iteration works", {
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
      rows_used_for_imputation = "all",
      update_ds_model = "every_iteration",
      M = is.na(df_XYZ_10_mis)
    )
  )
})


# Tests for update model everytime and model ds everytime ---------------------
test_that("all columns and (complete and partly_complete)
          rows works with updates every time", {
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
      update_model = "everytime",
      update_ds_model = "everytime",
      M = is.na(df_XYZ_10_mis)
    )
  )
  # if all columns are considered,
  # partly_complete are only complete observed rows
  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "partly_complete",
      update_model = "everytime",
      update_ds_model = "everytime",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

# Tests for update model everytime and model ds each_column -------------------
test_that("update model everytime and model ds each_column throws an error", {
  expect_error(
    impute_supervised(
      df_XYZ_10_mis,
      update_model = "everytime",
      update_ds_model = "each_column"
    ),
    "Combination of update model and data set is not implemented."
  )
})

# Tests for update model everytime and model ds every_iteration ---------------

test_that("all_no_update columns and
          (complete and partly_complete) rows works", {
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
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "only_complete",
      update_model = "everytime",
      update_ds_model = "every_iteration",
      M = is.na(df_XYZ_10_mis)
    )
  )

  # if all columns are considered,
  # partly_complete are only complete observed rows
  expect_equal(
    ds_imp_test,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "partly_complete",
      update_model = "everytime",
      update_ds_model = "every_iteration",
      M = is.na(df_XYZ_10_mis)
    )
  )
})

test_that("update model everytime and model ds every_iteration  works", {
  ds_imp_test <- df_XYZ_10
  M <- is.na(df_XYZ_10_mis)
  lm_x <- lm(X ~ Z + Y, df_XYZ_10, na.action = na.fail)
  ds_imp_test$X[M[, "X"]] <- predict(lm_x, df_XYZ_10[M[, "X"], ])
  lm_y <- lm(Y ~ Z + X, df_XYZ_10, na.action = na.fail)
  ds_imp_test$Y[M[, "Y"]] <- predict(lm_y, df_XYZ_10[M[, "Y"], ])

  ds_imp_sup <- impute_supervised(
    df_XYZ_10, # use "completed" ds and M
    cols_used_for_imputation = "all",
    rows_used_for_imputation = "all",
    update_model = "everytime",
    update_ds_model = "every_iteration",
    M = is.na(df_XYZ_10_mis)
  )
  expect_equal(
    ds_imp_sup,
    ds_imp_test
  )
  # There is a real difference between
  # update_ds_model evertime and every_iteration:
  expect_false(isTRUE(all.equal(
    ds_imp_sup,
    impute_supervised(
      df_XYZ_10, # use "completed" ds and M
      cols_used_for_imputation = "all",
      rows_used_for_imputation = "all",
      update_model = "everytime",
      update_ds_model = "everytime",
      M = is.na(df_XYZ_10_mis)
    )
  )))
})

# Test for update_model = every_iteration -------------------------------------

test_that("update_model every_iteration works", {
  expect_equal(
    impute_supervised(
      df_XYZ_10_mis,
      update_model = "every_iteration"
    ),
    impute_supervised(
      df_XYZ_10_mis,
      update_model = "each_column"
    )
  )
})
