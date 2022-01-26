#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param beta
#' @param sigma
#' @param meshblocks
#' @param rt_scat_positivity
#' @param cutoff_distance
#' @return
#' @author Nick Golding
#' @export
project_incidence <- function(beta, sigma, meshblocks, rt_scat_positivity, cutoff_distance) {
  
  # compute distance matrix d_ij between meshblock locations and possum scat
  # sampling locations in each survey, in km
  distance <- get_distance(meshblocks, rt_scat_positivity)
  
  mu_positivity <- as_data(rt_scat_positivity$mu_positive)
  
  # do sparse matrix-multiply with distance cutoff for computational efficiency
  keep_idx <- which(distance <= cutoff_distance, arr.ind = TRUE)
  row_idx <- keep_idx[, 1]
  col_idx <- keep_idx[, 2]
  distance_sparse <- distance[keep_idx]
  
  # compute unnormalised weights
  weights_raw_sparse <- exp(-0.5 * (distance_sparse / sigma) ^ 2)
  # normalise them (so prediction does not depend on number of samples nearby)
  weights_raw_sum_sparse <- tapply(weights_raw_sparse, row_idx, FUN = "sum")
  weights_sparse <- weights_raw_sparse / weights_raw_sum_sparse[row_idx]
  # multiply weights by covariate and aggregate
  weighted_positivity_elements_sparse <- weights_sparse * mu_positivity[col_idx]
  weighted_positivity <- tapply(weighted_positivity_elements_sparse, row_idx, FUN = "sum")
  
  # # confirm this gives the same answer as the matrix-multiply version
  # mask <- distance <= cutoff_distance
  # weights_raw <- exp(-0.5 * (distance / sigma) ^ 2) * mask
  # weights <- sweep(weights_raw, 1, rowSums(weights_raw), FUN = "/")
  # weighted_positivity_dense <- weights %*% mu_positivity
  # diff <- max(abs(weighted_positivity - weighted_positivity_dense))
  # calculate(diff, nsim = 10)
  
  incidence <- beta * weighted_positivity
  
  incidence
  
}
