test_that("model_donor throws an error if model_arg is not a list", {
  expect_error(
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2, "knn_complete_rows"),
    "model_arg must be a list or NULL"
  )
})

test_that("model_donor throw an error for i = NULL and !=complete_rows", {
  expect_error(
    model_donor(
      df_XYZ_10_mis, is.na(df_XYZ_10_mis), NULL,
      list(selection = "partly_complete_rows")
    ),
    "only donor selection \"complete_rows\"' is possible for this case"
  )
})

test_that("model_donor() works with model_arg = NULL", {
  expect_equal(
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), NULL, NULL),
    structure(df_XYZ_10_mis[stats::complete.cases(df_XYZ_10_mis), ],
      donor_selection = "complete_rows"
    )
  )
})


test_that("model_donor complete_rows works", {
  hd_model <- model_donor(
    df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2, list(selection = "complete_rows")
  )
  expect_true(all(
    rep.int(predict_donor(
      hd_model, df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2) %in%
        seq_len(10)[-c(2, 7)], 50)
  ))
})

test_that("model_donor knn_complete_rows works", {
  comp_cases <- df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), ]
  top_3 <- gower::gower_topn(df_XYZ_10_mis[2, ], comp_cases, n = 3)$index[, 1]
  expect_equal(
    structure(comp_cases[top_3, ], donor_selection = "knn_complete_rows"),
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2,
                list(selection = "knn_complete_rows", k = 3))
  )
})

test_that("model_donor partly_complete_rows works", {
  expect_equal(
    structure(df_XYZ_10_mis[!is.na(df_XYZ_10_mis$X), ],
              donor_selection = "partly_complete_rows"),
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), i = 2,
                list(selection = "partly_complete_rows"))
  )

  expect_equal(
    structure(df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), ],
              donor_selection = "partly_complete_rows"),
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), i = 7,
                list(selection = "partly_complete_rows"))
  )
})

test_that("model_donor knn_partly_complete_rows works", {
  cases_comp_Y <- df_XYZ_10_mis[!is.na(df_XYZ_10_mis$Y), ]
  top_3 <- gower::gower_topn(df_XYZ_10_mis[4, ], cases_comp_Y, n = 3)
  top_3 <- top_3$index[, 1]
  expect_equal(
    structure(cases_comp_Y[top_3, ],
              donor_selection = "knn_partly_complete_rows"),
    model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 4,
                list(selection = "knn_partly_complete_rows", k = 3))
  )
})

test_that("predict_donor average works", {
  donors_i_2 <- model_donor(df_XYZ_10_mis, is.na(df_XYZ_10_mis), 2,
                            list(selection = "complete_rows"))
  expect_equal(
    predict_donor(
      donors_i_2, df_XYZ_10_mis, M = is.na(df_XYZ_10_mis), i = 2, "average"
    ),
    colMeans(df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), "X", drop = FALSE])
  )
  donors_i_7 <- model_donor(
    df_XYZ_10_mis, is.na(df_XYZ_10_mis), 7, list(selection = "complete_rows")
  )
  expect_equal(
    predict_donor(
      donors_i_7, df_XYZ_10_mis, M = is.na(df_XYZ_10_mis), i = 7, "average"
    ),
    colMeans(df_XYZ_10_mis[complete.cases(df_XYZ_10_mis), c("X", "Y")])
  )
})
