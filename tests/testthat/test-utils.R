# do_not_stop_iter() ----------------------------------------------------------
test_that("do_not_stop_iter() works", {
  expect_equal(
    do_not_stop_iter(),
    list(stop_iter = FALSE)
  )
})

# get_col_indices() -----------------------------------------------------------
test_that("get_col_indices() works with only_complete", {
  expect_equal(
    get_col_indices("only_complete", M1, M2, 1),
    integer(0)
  )
  expect_equal(
    get_col_indices("only_complete", M2, M1, 1),
    2
  )
})

test_that("get_col_indices() works with already_imputed", {
  expect_equal(
    get_col_indices("already_imputed", M1, M2, 1),
    2
  )
  expect_equal(
    get_col_indices("already_imputed", M2, M1, 1),
    integer(0)
  )
})

test_that("get_col_indices() works with all", {
  expect_equal(
    get_col_indices("all", M1, M2, 1),
    c(2, 3)
  )
  expect_equal(
    get_col_indices("all", M2, M1, 2),
    c(1, 3)
  )
})

test_that("get_col_indices() errors for invalid option", {
  expect_error(
    get_col_indices("asdf", M1, M2, 1),
    "'asdf' is not a valid option for cols_used_for_imputation"
  )
})


# get_row_indices() -----------------------------------------------------------
test_that("get_row_indices() works with only_complete", {
  expect_equal(
    get_row_indices("only_complete", M_start = M1),
    8
  )
  expect_equal(
    get_row_indices("only_complete", M_start = M1[c(8, 1, 8, 3, 7, 8, 4, 5), ]),
    c(1, 3, 6)
  )
})

test_that("get_row_indices() works with partly_complete", {
  expect_equal(
    get_row_indices("partly_complete", M_start = M1, k = 1, cols_used_imp = 2),
    which(!apply(M1[, c(1, 2)], 1, any))
  )
  expect_equal(
    get_row_indices("partly_complete", M_start = M1, k = 3, cols_used_imp = 2),
    which(!apply(M1[, c(2, 3)], 1, any))
  )
  expect_equal(
    get_row_indices("partly_complete", M_start = M1, k = 3, cols_used_imp = 1),
    which(!apply(M1[, c(1, 3)], 1, any))
  )
})


test_that("get_row_indices() works with complete_in_k", {
  expect_equal(
    get_row_indices("complete_in_k", M_start = M1, k = 1),
    which(!M1[, 1], 1, any)
  )
  expect_equal(
    get_row_indices("complete_in_k", M_start = M1, k = 2),
    which(!M1[, 2], 1, any)
  )
  expect_equal(
    get_row_indices("complete_in_k", M1, k = 3),
    which(!M1[, 3], 1, any)
  )
})

test_that("get_row_indices() works with already_imputed", {
  expect_equal(
    get_row_indices("already_imputed", M = M1, k = 1, cols_used_imp = 2),
    which(!apply(M1[, c(1, 2)], 1, any))
  )
  expect_equal(
    get_row_indices("already_imputed", M = M1, k = 3, cols_used_imp = 2),
    which(!apply(M1[, c(2, 3)], 1, any))
  )
  expect_equal(
    get_row_indices("already_imputed", M = M1, k = 3, cols_used_imp = 1),
    which(!apply(M1[, c(1, 3)], 1, any))
  )
})

test_that("get_row_indices() works with all_except_i", {
  expect_equal(
    get_row_indices("all_except_i", M = M1, i = 1),
    2:8
  )
  expect_equal(
    get_row_indices("all_except_i", M = M1, i = 3),
    c(1, 2, 4:8)
  )
  expect_equal(
    get_row_indices("all_except_i", M = M1, i = 8),
    1:7
  )
})

test_that("get_row_indices() works with all", {
  expect_equal(
    get_row_indices("all", M = M1),
    1:8
  )
  expect_equal(
    get_row_indices("all", M = M1[c(1:5, 8:3), ]),
    seq_along(c(1:5, 8:3))
  )
})


test_that("get_row_indices() errors for wrong option", {
  expect_error(
    get_row_indices("asdf", M = M1),
    "'asdf' is not a valid option for rows_used_for_imputation"
  )
})

# check_update_combinations() -------------------------------------------------
test_that("check_update_combinations() works", {
  test_fun <- function(update_model, update_ds_model,
                       rows_used_for_imputation) {
    check_update_combinations(
      update_model, update_ds_model, rows_used_for_imputation
    )
    1 + 1
    list(update_model, update_ds_model, rows_used_for_imputation)
  }

  expect_equal(
    suppressWarnings(test_fun("each_column", "asdf", "all_except_i")),
    list("everytime", "asdf", "all_except_i")
  )
})

# set_defaults_for_missing() --------------------------------------------------
test_that("set_defaults_for_missing() works", {
  expect_equal(
    set_defaults_for_missing(list(a = 1, b = 5), list(a = 3, b = 2, c = 9)),
    list(a = 1, b = 5, c = 9)
  )
  expect_equal(
    set_defaults_for_missing(NULL, list(b = 3, d = 2)),
    list(b = 3, d = 2)
  )
})
