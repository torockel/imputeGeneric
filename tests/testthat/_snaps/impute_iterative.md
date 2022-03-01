# Warning for incomplete ds is shown (only once)

    Code
      impute_iterative(df_XYZ_10_mis, max_iter = 2, rows_used_for_imputation = "all",
        cols_used_for_imputation = "all")
    Warning <simpleWarning>
      Imputation is not complete. There are still missing values in `ds`.
    Output
                 X         Y Z
      1   1.000000 0.7725428 5
      2   1.400577 1.0000000 5
      3   3.000000 1.0000000 4
      4   4.000000 1.5164418 4
      5   5.000000 1.0000000 3
      6   6.000000 2.0000000 3
      7         NA        NA 2
      8   8.000000 2.0000000 2
      9   9.000000 2.0000000 1
      10 10.000000 2.0000000 1

