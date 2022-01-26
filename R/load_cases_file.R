#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_file_to_2019
#' @return
#' @author Nick Golding
#' @export
load_cases_file <- function(cases_file) {

  cases_file %>%
    readxl::read_excel() %>%
    select(
      MB2011,
      likely_exposure,
      date = date_sxonset,
      type_of_contact = typecontact0,
      exposure_MB2011 = mb0
    )
}
