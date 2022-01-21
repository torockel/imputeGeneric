test_that("complete columns and rows works", {
  ds_imp <- impute_cols_seq(
    df_XY_10_X_mis,
    cols_used_for_imputation = "only_complete",
    rows_used_for_imputation = "only_complete")
  res_ds <- df_XY_10_X_mis
  res_ds[c(2, 7), "X"] <- c(3.25, 8.25)
  expect_equal(ds_imp, res_ds)
})
