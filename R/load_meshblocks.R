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
  mb <- mb_file %>%
    st_read() %>%
    rename(
      population = X2011.cen_1,
      dwellings = X2011.censu
    ) %>%
    filter(
      SA2_NAME11 %in% c("Point Nepean", "Rosebud - McCrae")
    ) %>%
    st_transform(
      28355
    )
  
  mb_2011_2016_lookup <- get_mb_2011_2016_lookup()
  
  pop_2016 <- read_csv(
    "data/2016 census mesh block counts.csv",
    col_types = cols(
      MB_CODE_2016 = col_character(),
      MB_CATEGORY_NAME_2016 = col_character(),
      AREA_ALBERS_SQKM = col_double(),
      Dwelling = col_double(),
      Person = col_double(),
      State = col_double()
    )
  )
  
  mb %>%
    left_join(
      mb_2011_2016_lookup, 
      by = "MB_CODE11"
    ) %>%
    left_join(
      pop_2016,
      by = "MB_CODE_2016"
    ) %>%
    mutate(
      # use the 2016 populations where available, and where not (those 2011
      # meshblocks not in the correspondence) use the 2011 populations
      Population = coalesce(Person, population),
      weight = replace_na(weight, 1)
    ) %>%
    group_by(
      MB_CODE11
    ) %>%
    summarise(
      Population = weighted.mean(Population, weight),
      across(
        c(ALBERS_SQM, Latitude, Longitude),
        first
      )
    )
  
}
