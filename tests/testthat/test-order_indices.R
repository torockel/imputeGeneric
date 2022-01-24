test_that("order_indices lowest_md_first works", {
  expect_equal(
    order_indices(df_XYZ_10_mis, "lowest_md_first", dimension = "cols"),
    c(3, 1, 2)
  )
})

test_that("order_indices highest_md_first works", {
  expect_equal(
    order_indices(df_XYZ_10_mis, "highest_md_first", dimension = "cols"),
    c(2, 1, 3)
  )
})

test_that("order_indices increasing_index works", {
  expect_equal(
    order_indices(df_XYZ_10_mis, "increasing_index", dimension = "cols"),
    1:3
  )
})

test_that("order_indices decreasing_index works", {
  expect_equal(
    order_indices(df_XYZ_10_mis, "decreasing_index", dimension = "cols"),
    3:1
  )
})

test_that("order_indices decreasing_index works with rows", {
  expect_equal(
    order_indices(df_XYZ_10_mis, "decreasing_index", dimension = "rows"),
    10:1
  )
})

test_that("order_indices throws an error for wrong order_option", {
  expect_error(
    order_indices(df_XYZ_10_mis, "IDoNotExist", "rows"),
    "`IDoNotExist` is not a valid option for ordering."
  )
})

test_that("order_rows works", {
  expect_equal(
    order_rows(df_XYZ_10_mis, "increasing_index"),
    1:10
  )
})

test_that("order_cols works", {
  expect_equal(
    order_cols(df_XYZ_10_mis, "increasing_index"),
    1:3
  )
})
