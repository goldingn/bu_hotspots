#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meshblock_incidence
#' @param rt_scat_positivity
#' @return
#' @author Nick Golding
#' @export
train_model <- function(meshblock_incidence, rt_scat_positivity, cutoff_distance = 1) {
  
  # split incidence and scat positivity into summer and winter surveys
  meshblock_incidence_summer <- meshblock_incidence %>%
    filter(
      period == "summer"
    )
  
  rt_scat_positivity_summer <- rt_scat_positivity %>%
    filter(
      period == "summer"
    )
  
  meshblock_incidence_winter <- meshblock_incidence %>%
    filter(
      period == "winter"
    )
  
  rt_scat_positivity_winter <- rt_scat_positivity %>%
    filter(
      period == "winter"
    )
  
  # define a vague-ish positive prior on the distance decay, with most mass at 0
  # (slightly favouring shorter decays)
  sigma <- normal(0, cutoff_distance / 2, truncation = c(0, Inf))
  # 2 * (1 - pnorm(cutoff_distance, 0, cutoff_distance / 2))
  # 5% chance *a priori* that the sigma goes beyond the cutoff distance
  
  # scaling parameter on the FOI
  beta <- normal(0, 1, truncation = c(0, Inf))
  
  # define likelihoods on the two seasons separately
  likelihood_summer <- define_likelihood(
    meshblock_incidence = meshblock_incidence_summer,
    rt_scat_positivity = rt_scat_positivity_summer,
    beta = beta,
    sigma = sigma,
    cutoff_distance = cutoff_distance
  )
  likelihood_winter <- define_likelihood(
    meshblock_incidence = meshblock_incidence_winter,
    rt_scat_positivity = rt_scat_positivity_winter,
    beta = beta,
    sigma = sigma,
    cutoff_distance = cutoff_distance
  )
  
  m <- model(beta, sigma)
  draws <- mcmc(m)
  
  list(
    model = m,
    draws = draws,
    likelihoods = list(
      summer = likelihood_summer,
      winter = likelihood_winter
    ),
    parameters = list(
      beta = beta,
      sigma = sigma
    )
  )
  
}