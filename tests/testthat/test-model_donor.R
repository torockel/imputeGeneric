test_that("model_donor simultan_complete works", {
  hd_model <- model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2, donor_selection = "simultan_complete")
  expect_true(all(
    rep.int(predict_donor(hd_model, df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2) %in%
            seq_len(10)[-c(2, 7)], 50)
  ))
})

# Add tests for simultan_incomplete and knn_simultan_incomplete

test_that("model_donor knn_simultan_complete works", {
  comp_cases <- df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), ]
  top_3 <- gower::gower_topn(df_XYZ_10_mis[2, ], comp_cases, n = 3)$index[, 1]
  expect_equal(
    structure(comp_cases[top_3, ], donor_selection = "knn_simultan_complete"),
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2, donor_selection = "knn_simultan_complete", donor_k = 3)
  )
})

test_that("predict_donor average works", {
  donors_i_2 <- model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2, donor_selection = "simultan_complete")
  expect_equal(
    predict_donor(donors_i_2, df_XYZ_10_mis, M = is.na(df_XYZ_10_mis), i = 2, "average"),
    colMeans(df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), "X", drop = FALSE])
  )
  donors_i_7 <- model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 7, donor_selection = "simultan_complete")
  expect_equal(
    predict_donor(donors_i_7, df_XYZ_10_mis, M = is.na(df_XYZ_10_mis), i = 7, "average"),
    colMeans(df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), c("X", "Y")])
  )
})
