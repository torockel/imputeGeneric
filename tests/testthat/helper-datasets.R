# Define some complete data.frames for testing ------------------------------

df_XYZ_10 <- data.frame(
  X = 1:10, Y = rep(c(1, 2), each = 5), Z = rep(5:1, each = 2)
)

# Define some incomplete data.frames for testing ------------------------------
df_XYZ_10_mis <- df_XYZ_10
df_XYZ_10_mis[c(2, 7), "X"] <- NA
df_XYZ_10_mis[c(1, 4, 7), "Y"] <- NA


# info_list for testing stop funs ---------------------------------------------
info_list <- list(M = is.na(df_XYZ_10_mis), nr_iterations = 42, max_iter = 70)
df_XYZ_10_stop <- structure(df_XYZ_10, nr_iterations = info_list$nr_iterations)


# MD-Indicator matrices -------------------------------------------------------
M1 <- matrix(c(
  TRUE, TRUE, TRUE,
  TRUE, TRUE, FALSE,
  TRUE, FALSE, TRUE,
  TRUE, FALSE, FALSE,
  FALSE, TRUE, TRUE,
  FALSE, TRUE, FALSE,
  FALSE, FALSE, TRUE,
  FALSE, FALSE, FALSE
),
ncol = 3, byrow = TRUE
)
M2 <- matrix(c(
  FALSE, FALSE, TRUE,
  TRUE, FALSE, FALSE,
  FALSE, FALSE, FALSE
),
ncol = 3, byrow = TRUE
)
