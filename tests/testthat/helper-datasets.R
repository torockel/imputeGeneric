# Define some complete data.frames for testing ------------------------------

df_XY_10 <- data.frame(X = 1:10, Y = rep(c(1, 2), each = 5))

# Define some incomplete data.frames for testing ------------------------------
df_XY_10_X_mis <- df_XY_10
df_XY_10_X_mis[c(2, 7), "X"] <- NA
