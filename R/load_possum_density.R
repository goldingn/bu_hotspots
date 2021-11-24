#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
load_possum_density <- function() {

  # load the possum density raster
  possum_density <- raster("data/pred_abund_quadcorrect_28May.tif")
  
  # clip it to the study area
  clip_area <- c(
    xmin = 144.700862569109,
    xmax = 144.94087901,
    ymin = -38.43319228, 
    ymax = -38.3154045783998
  )
  
  crop(
    possum_density,
    clip_area
  )

}
