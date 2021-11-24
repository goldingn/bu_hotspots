#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
load_cases <- function() {

  cases_file <- "data/BU NHMRC NickGolding SpatialEpi data NO PHESSID_Jan to 9 Nov 2021.xlsx"
  cases_file %>%
    readxl::read_excel() %>%
    select(
      MB2011,
      likely_exposure,
      date = date_sxonset,
      type_of_contact = typecontact0,
      exposure_MB2011 = mb0
    ) %>%
    mutate(
      date = as.Date(date),
      exposure_MB2011 = as.character(exposure_MB2011)
    ) %>%
    filter(
      type_of_contact == "Resident" | !is.na(exposure_MB2011)
    ) %>%
    mutate(
      MB2011 = coalesce(exposure_MB2011, MB2011) 
    ) %>%
    select(
      MB2011,
      date,
      likely_exposure
    )

}
