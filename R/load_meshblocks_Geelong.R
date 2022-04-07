#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
load_meshblocks_Geelong <- function() {
  
#  mb_file <- "data/MB_2011_VIC_Census_counts_Centroids_LatLong_MorPen_Only/MB_2011_VIC_Census_counts_Centroids_LatLong_MorPen_Only.shp"
  mb_file <- "data/MB_2011_VIC_Census_counts_Centroids_LatLong/MB_2011_VIC_Census_counts_Centroids_LatLong.shp"
  mb_file %>%
    st_read() %>%
        rename(
      Population = X2011.cen_1,
      Dwellings = X2011.censu
    ) %>%
    filter(
#      SA2_NAME11 %in% c("Point Nepean", "Rosebud - McCrae")
#      SA2_NAME11 %in% c("Point Nepean", "Rosebud - McCrae", "Belmont", "Corio - Norlane", "Geelong", "Geelong West - Hamlyn Heights", "Grovedale", "Highton", "Newcomb - Moolap", "Newtown (Vic.)", "North Geelong - Bell Park")
      SA2_NAME11 %in% c("Belmont", "Corio - Norlane", "Geelong", "Geelong West - Hamlyn Heights", "Grovedale", "Highton", "Newcomb - Moolap", "Newtown (Vic.)", "North Geelong - Bell Park")
      
    ) %>%
    st_transform(
      28355
    )
  
}
