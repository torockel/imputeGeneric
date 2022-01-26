model_donor <- function(ds, M, i, donor_selection = "simultan_complete") {
  if (donor_selection == "simultan_complete") {
    suitable_rows <- complete.cases(ds)
  } else if(donor_selection == "simultan_incomplete") {
    suitable_rows <- apply(M, 1, function(x) !any(M[i, ] & x))
  } else {
    stop(paste0("'", donor_selection, "' is not a valid option for donor_selection"))
  }
  return(structure(ds[suitable_rows, ], donor_selection = donor_selection))
}

predict_donor <- function(ds_donors, ds_used, M, i, donor_aggregation = "choose_random") {
  if (donor_aggregation == "choose_random") {
    return(ds_donors[sample.int(nrow(ds_donors), 1), M[i, ]])
  } else if (donor_aggregation == "average") {
    return(colMeans(ds_donors[ , M[i, ], drop = FALSE]))
  }
}
