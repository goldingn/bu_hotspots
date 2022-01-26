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
  meshblock_incidence <- filter_by_distance(
      meshblock_incidence,
      cutoff_distance,
      positive_scats
    )
  
  rt_scat_positivity <- filter_by_distance(
    rt_scat_positivity,
    cutoff_distance,
    meshblock_incidence
  )
  
  incidence <- project_incidence(
    beta = beta,
    sigma = sigma,
    meshblocks = meshblock_incidence,
    rt_scat_positivity = rt_scat_positivity,
    cutoff_distance = cutoff_distance
  )
  
  expected_cases <- incidence * meshblock_incidence$pop
  
  distribution(meshblock_incidence$cases) <- poisson(expected_cases)
  
  # return various objects
  list(
    greta_arrays = list(
      incidence = incidence,
      expected_cases = expected_cases
    ),
    data = list(
      rt_scat_positivity = rt_scat_positivity,
      meshblock_incidence = meshblock_incidence,
      distance = distance,
      cutoff_distance = cutoff_distance
    )
  )
  
}