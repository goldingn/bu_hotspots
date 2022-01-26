#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fitted_model
#' @return
#' @author Nick Golding
#' @export
check_fitted_model <- function(fitted_model) {
  
  # plot draws
  plot(fitted_model$draws)
  
  # get the R hat statistics for the parameters
  r_hat <- coda::gelman.diag(fitted_model$draws, autoburnin = FALSE, multivariate = FALSE)
  
  
  # get combined incidence and expected cases for plotting
  incidence <- rbind(
    fitted_model$likelihoods$summer$greta_arrays$incidence,
    fitted_model$likelihoods$winter$greta_arrays$incidence
  )
  expected_cases <- rbind(
    fitted_model$likelihoods$summer$greta_arrays$expected_cases,
    fitted_model$likelihoods$winter$greta_arrays$expected_cases
  )
  
  data <- rbind(
    fitted_model$likelihoods$summer$data$meshblock_incidence,
    fitted_model$likelihoods$winter$data$meshblock_incidence
  )
  
  # get posterior means of the predicted incidence and cases
  incidence_posterior <- calculate(
    incidence,
    values = fitted_model$draws,
    nsim = 1000
  )[[1]]
  incidence_posterior_mean <- colMeans(incidence_posterior[, , 1])
  
  expected_cases_posterior <- calculate(
    expected_cases,
    values = fitted_model$draws,
    nsim = 1000
  )[[1]]
  expected_cases_posterior_mean <- colMeans(expected_cases_posterior[, , 1])
  
  # simulate cases for PPCs and exceedance probabilities
  cases_new <- poisson(expected_cases)
  cases_sim <- calculate(
    cases_new,
    values = fitted_model$draws,
    nsim = 1000
  )[[1]][, , 1]
  
  p_some_cases <- colMeans(cases_sim > 0)
  
  # add these on and return
  data %>%
    mutate(
      predicted_incidence = incidence_posterior_mean,
      predicted_cases = expected_cases_posterior_mean,
      probability_of_cases = p_some_cases,
      sim_cases_1 = cases_sim[sample.int(1000, 1), ],
      sim_cases_2 = cases_sim[sample.int(1000, 1), ],
      sim_cases_3 = cases_sim[sample.int(1000, 1), ],
    )
  
}
