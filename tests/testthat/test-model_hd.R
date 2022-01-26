test_that("random_simultan_complete works", {
  hd_model <- model_hot_deck(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2, hd_type = "random_simultan_complete")
  expect_true(all(
    rep.int(predict_hot_deck(hd_model, df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2) %in%
            seq_len(10)[-c(2, 7)], 50)
  ))
})
