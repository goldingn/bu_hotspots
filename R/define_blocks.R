#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meshblock_incidence_survey_periods
#' @param n_blocks
#' @return
#' @author Nick Golding
#' @export
# define spatially contiguous blocks for spatial cross-validation
define_blocks <- function(meshblocks, n_blocks = 3) {
  
  # k-means clustering on coordinates, weighted by incidence, to define blocks
#  clustering <- meshblocks %>%
#    mutate(
#      scaled_incidence = round(3000 * incidence)
#    ) %>%
#    uncount(scaled_incidence) %>%
#    st_coordinates() %>%
#    stats::kmeans(
#      centers = n_blocks
#    )
  
  
  # Andrew B
  clustering <- meshblocks %>%
    mutate(
      scaled_incidence = round(3000 * incidence)
    ) %>%
    uncount(scaled_incidence) %>%
    st_coordinates() %>%
    kcca(k=n_blocks, kccaFamily("kmeans")
  )
  
  # add the blocks onto the training data and return
#  meshblocks %>%
#    dplyr::mutate(
#      block = predict(
#        data = clustering,
#        newdata = st_coordinates(.),
#        method = "classes"
#      )
#    )
  
  # Abuul
  meshblocks %>%
    dplyr::mutate(
      block = flexclust::predict(
        clustering,
        newdata = st_coordinates(.)
      )
    )
  
}