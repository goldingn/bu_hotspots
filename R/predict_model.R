#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fitted_model
#' @param meshblocks
#' @param rt_scat_positivity
#' @return
#' @author Nick Golding
#' @export
predict_model <- function(
  fitted_model,
  meshblocks,
  rt_scat_positivity,
  n_sims = 1000,
  cutoff_distance = Inf
) {
  
  # if a cutoff_distance, need to trim predictable locations here
  if (cutoff_distance < Inf) {
    
    positive_scats <- rt_scat_positivity %>%
      filter(mu_positive)
    
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
    
  }
  
  beta <- fitted_model$parameters$beta
  sigma <- fitted_model$parameters$sigma
  draws <- fitted_model$draws
  
  incidence <- project_incidence(
    beta = beta,
    sigma = sigma,
    meshblocks = meshblocks,
    rt_scat_positivity = rt_scat_positivity,
    cutoff_distance = cutoff_distance
  )
  
  incidence_posterior <- calculate(
    incidence,
    values = fitted_model$draws,
    nsim = n_sims
  )[[1]]
  
  incidence_posterior_quants <- apply(
    incidence_posterior[, , 1],
    2,
    quantile,
    c(0.025, 0.075)
  )
  
  meshblocks %>%
    mutate(
      incidence_pred_mean = colMeans(incidence_posterior[, , 1]),
      incidence_pred_lower = incidence_posterior_quants[1, ],
      incidence_pred_upper = incidence_posterior_quants[2, ]
    )
  
}

