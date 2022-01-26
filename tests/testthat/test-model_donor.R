test_that("donor simultan_complete works", {
  hd_model <- model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2, donor_selection = "simultan_complete")
  expect_true(all(
    rep.int(predict_donor(hd_model, df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2) %in%
            seq_len(10)[-c(2, 7)], 50)
  ))
})
