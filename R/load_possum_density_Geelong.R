#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
load_possum_density_Geelong <- function() {

  # load the possum density raster
  possum_density <- raster("data/pred_abund_quadcorrect_28May.tif")
  
  # clip it to the study area
  clip_area <- c(
    xmin = 144.251030,
    xmax = 144.424197,
    ymin = -38.245875, 
    ymax = -38.068301
    #ymax = -38.001333
  )
  
  crop(
    possum_density,
    clip_area
  )

}
