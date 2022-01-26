#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param parameters
#' @param meshblock_incidence_summer
#' @param rt_scat_positivity_summer
#' @return
#' @author Nick Golding
#' @export
# given incidence and scat positivity, define a likelihood component
define_likelihood <- function(meshblock_incidence,
                              rt_scat_positivity,
                              beta,
                              sigma,
                              cutoff_distance = 1) {
  
  positive_scats <- rt_scat_positivity %>%
    filter(mu_positive)
  
  # remove any meshblock (or scat) datapoint that is more than 1km from the
  # nearest other type of datapoint (drops 2 cases, no scats)
  meshblock_incidence <- meshblock_incidence %>%
    filter_by_distance(
      cutoff_distance,
      positive_scats
    )
  
  rt_scat_positivity <- rt_scat_positivity %>%
    filter_by_distance(
      cutoff_distance,
      meshblock_incidence
    )
  
  # compute distance matrix d_ij between meshblock locations and possum scat
  # sampling locations in each survey, in km
  distance <- get_distance(meshblock_incidence, rt_scat_positivity)
  
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
  expected_cases <- incidence * meshblock_incidence$pop
  
  distribution(meshblock_incidence$cases) <- poisson(expected_cases)
  
  # return various objects
  list(
    greta_arrays = list(
      incidence = incidence,
      expected_cases = expected_cases,
      mu_positivity = mu_positivity
    ),
    data = list(
      rt_scat_positivity = rt_scat_positivity,
      meshblock_incidence = meshblock_incidence,
      distance = distance,
      cutoff_distance = cutoff_distance
    )
  )
  
}