#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param case_count_previous_block
#' @return
#' @author Nick Golding
#' @export
# the probability of observing any cases, givent he expected number of cases,
# under a poisson sampling assumption
prob_any_cases <- function(expected_cases) {
  1 - dpois(0, expected_cases)
}