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
get_distance <- function(x, y) {

  distance <- sf::st_distance(
    x,
    y,
    which = "Euclidean",
  )
  units(distance) <- "km"
  units(distance) <- NULL
  distance
  
}
