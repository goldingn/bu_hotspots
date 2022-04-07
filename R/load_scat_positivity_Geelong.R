#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
load_scat_positivity_Geelong <- function() {
  
  readxl::read_excel("data/20210811_Extract4Nick_GEELONG_ONLY.xlsx")  %>%
#  readxl::read_excel("data/20210811_Extract4Nick.xlsx")  %>%
    rename_with(
      tolower
    ) %>%
    mutate(
      date = as.Date(
        start_survey,
        format = "%a %b %d %H:%M:%S UTC %Y"
      ),
      species = case_when(
        suspected_organism == 1 ~ "ringtail",
        suspected_organism == 2 ~ "brushtail",
        TRUE ~ NA_character_
      ),
      mu_positive = `cq(is2404)` != "NEG"
    ) %>%
    filter(
      !is.na(date),
      found_pellets == 1
    ) %>%
    dplyr::select(
      date,
      latitude,
      longitude,
      species,
      mu_positive
    ) %>%
    arrange(
      mu_positive
    )
  
  
}
