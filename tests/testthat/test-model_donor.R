test_that("model_donor complete_rows works", {
  hd_model <- model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2, donor_selection = "complete_rows")
  expect_true(all(
    rep.int(predict_donor(hd_model, df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2) %in%
            seq_len(10)[-c(2, 7)], 50)
  ))
})

test_that("model_donor knn_complete_rows works", {
  comp_cases <- df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), ]
  top_3 <- gower::gower_topn(df_XYZ_10_mis[2, ], comp_cases, n = 3)$index[, 1]
  expect_equal(
    structure(comp_cases[top_3, ], donor_selection = "knn_complete_rows"),
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2, donor_selection = "knn_complete_rows", donor_k = 3)
  )
})

test_that("model_donor partly_complete_rows works", {
  expect_equal(
    structure(df_XYZ_10_mis[!is.na(df_XYZ_10_mis$X), ], donor_selection = "partly_complete_rows"),
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), i = 2, donor_selection = "partly_complete_rows")
  )

  expect_equal(
    structure(df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), ], donor_selection = "partly_complete_rows"),
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), i = 7, donor_selection = "partly_complete_rows")
  )
})

test_that("model_donor knn_partly_complete_rows works", {
  cases_comp_Y <- df_XYZ_10_mis[!is.na(df_XYZ_10_mis$Y), ]
  top_3 <- gower::gower_topn(df_XYZ_10_mis[4, ], cases_comp_Y, n = 3)$index[, 1]
  expect_equal(
    structure(cases_comp_Y[top_3, ], donor_selection = "knn_partly_complete_rows"),
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 4, donor_selection = "knn_partly_complete_rows", donor_k = 3)
  )
})

test_that("predict_donor average works", {
  donors_i_2 <- model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2, donor_selection = "complete_rows")
  expect_equal(
    predict_donor(donors_i_2, df_XYZ_10_mis, M = is.na(df_XYZ_10_mis), i = 2, "average"),
    colMeans(df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), "X", drop = FALSE])
  )
  donors_i_7 <- model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 7, donor_selection = "complete_rows")
  expect_equal(
    predict_donor(donors_i_7, df_XYZ_10_mis, M = is.na(df_XYZ_10_mis), i = 7, "average"),
    colMeans(df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), c("X", "Y")])
  )
})
