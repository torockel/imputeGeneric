# Define some complete data.frames for testing ------------------------------

df_XYZ_10 <- data.frame(X = 1:10, Y = rep(c(1, 2), each = 5), Z = rep(5:1, each = 2))

# Define some incomplete data.frames for testing ------------------------------
df_XYZ_10_mis <- df_XYZ_10
df_XYZ_10_mis[c(2, 7), "X"] <- NA
df_XYZ_10_mis[c(1, 4, 7), "Y"] <- NA

df_XY_10_X_mis <- df_XYZ_10_mis[, c("X", "Y")]
