#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases
#' @param pred_cases
#' @return
#' @author Nick Golding
#' @export
# compute the deviance (similar to variance) between the true and predicted case
# counts
poisson_deviance <- function(truth, predicted) {
  -2 * sum(dpois(truth, predicted, log = TRUE))
}
