model_donor <- function(ds, M, i, donor_selection = "simultan_complete") {
  if (donor_selection == "simultan_complete") {
    suitable_rows <- complete.cases(ds)
  } else if(donor_selection == "simultan_incomplete") {
    suitable_rows <- apply(M, 1, function(x) !any(M[i, ] & x))
  }
  return(structure(ds[suitable_rows, ], donor_selection = donor_selection))
}

predict_donor <- function(model_hd, ds_used, M, i, donor_aggregation = "choose_random") {
  model_hd[sample.int(nrow(model_hd), 1), M[i, ]]
}
