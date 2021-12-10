#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
load_meshblocks <- function() {
  
  mb_file <- "data/MB_2011_VIC_Census_counts_Centroids_LatLong_MorPen_Only/MB_2011_VIC_Census_counts_Centroids_LatLong_MorPen_Only.shp"
  mb_file %>%
    st_read() %>%
    rename(
      Population = X2011.cen_1,
      Dwellings = X2011.censu
    ) %>%
    filter(
      SA2_NAME11 %in% c("Point Nepean", "Rosebud - McCrae")
    ) %>%
    st_transform(
      28355
    )
  
}
