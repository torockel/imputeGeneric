model_hot_deck <- function(ds, M, i, hd_type = "random_simultan_complete") {
  if (hd_type == "random_simultan_complete") {
    suitable_rows <- complete.cases(ds)
  } else if(hd_type == "random_simultan_incomplete") {
    suitable_rows <- apply(M, 1, function(x) !any(M[i, ] & x))
  }
  return(structure(ds[suitable_rows, ], hd_type = hd_type))
}

predict_hot_deck <- function(model_hd, ds_used, M, i) {
  model_hd[sample.int(nrow(model_hd), 1), M[i, ]]
}
