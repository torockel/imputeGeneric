model_donor <- function(ds, M, i, donor_selection = "simultan_complete", donor_k = 10) {
  if (donor_selection %in% c("simultan_complete", "knn_simultan_complete")) {
    suitable_rows <- complete.cases(ds)
  } else if(donor_selection %in% c("simultan_incomplete", "knn_simultan_incomplete")) {
    suitable_rows <- apply(M, 1, function(x) !any(M[i, ] & x))
  } else {
    stop(paste0("'", donor_selection, "' is not a valid option for donor_selection"))
  }
  if (donor_selection %in% c("knn_simultan_complete", "knn_simultan_incomplete")) {
    suitable_rows_ind <- which(suitable_rows)
    best_k <- gower::gower_topn(ds[i, ], ds[suitable_rows, ], n = donor_k)$index[, 1]
    suitable_rows <- suitable_rows_ind[best_k]
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
