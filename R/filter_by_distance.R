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
filter_by_distance <- function(data, cutoff_distance, positive_scats) {

  distance <- get_distance(data, positive_scats)
  keep_idx <- which(distance <= cutoff_distance, arr.ind = TRUE)
  rows_keep <- sort(unique(keep_idx[, 1]))
  data[rows_keep, ]

}
