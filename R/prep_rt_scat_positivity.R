#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param scat_positivity
#' @return
#' @author Nick Golding
#' @export
prep_rt_scat_positivity <- function(scat_positivity) {

  scat_positivity %>%
    filter(
      species == "ringtail"
    ) %>%
    select(
      -species
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
    # drop the sf structure
    st_set_geometry(NULL) %>%
    # standardise the dates and scale the coordinates
    mutate(
      date_num = as.numeric(date - min(date)),
      x_scaled = (x - mean(x)) / 1000,
      y_scaled = (y - mean(y)) / 1000,
    )

}
