#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scat_positivity
#' @return
#' @author Nick Golding
#' @export
prep_rt_scat_positivity <- function(scat_positivity, possum_density) {

  survey <- define_survey_periods()
  
  scat_positivity %>%
    filter(
      species == "ringtail"
    ) %>%
    select(
      -species
    ) %>%
    # extract average possum density in a 500m buffer of the point (accounts for
    # coastal errors in raster edge) with latlongs (safer than reprojecting the
    # raster)
    mutate(
      rt_possum_density = raster::extract(
        possum_density,
        select(., longitude, latitude),
        buffer = 500,
        small = TRUE,
        fun = "mean",
        na.rm = TRUE
      )
    ) %>%
    # coerce to an sf object
    st_as_sf(
      coords = c("longitude", "latitude")
    ) %>%
    # set these as lat-long
    st_set_crs(
      4326
    ) %>%
    # transform to MGA zone 55 (projected system for Melbourne area)
    st_transform(
      28355
    ) %>%
    # pull out the coordinates as columns
    mutate(
      x = st_coordinates(.)[, 1],
      y = st_coordinates(.)[, 2],
    ) %>%
    # standardise the dates and scale the coordinates
    mutate(
      date_num = as.numeric(date - min(date)),
      x_scaled = (x - mean(x)) / 1000,
      y_scaled = (y - mean(y)) / 1000
    ) %>%
    mutate(
      period = case_when(
        date >= survey$summer_start_date & date <= survey$summer_end_date ~ "summer",
        date >= survey$winter_start_date & date <= survey$winter_end_date ~ "winter",
        TRUE ~ "other"
      ),
      .before = everything()
    ) %>%
    filter(
      period != "other"
    )

}
