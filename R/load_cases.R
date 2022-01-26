#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
load_cases <- function() {
  
  # starts and ends of possum scat surveys
  survey <- define_survey_periods()
  
  # load all case files
  cases_file_to_2019 <- "data/BU NHMRC NickGolding SpatialEpi data NO PHESSID_upd 26 Nov 2019.xlsx"
  cases_file_2019_2020 <- "data/BU NHMRC NickGolding SpatialEpi data with NO PHESSID_2019 and 2020.xlsx"
  cases_file_2021 <- "data/BU NHMRC NickGolding SpatialEpi data NO PHESSID_Jan to 9 Nov 2021.xlsx"
  
  bind_rows(
    load_cases_file(cases_file_to_2019),
    load_cases_file(cases_file_2019_2020),
    load_cases_file(cases_file_2021)
  ) %>%
    mutate(
      exposure_MB2011 = as.character(exposure_MB2011),
      symptom_onset_date = as.Date(date),
      # incubation period (infection to onset) has a IQR (50% CI) of 101-171
      # days
      exposure_start_date = symptom_onset_date - 171,
      exposure_end_date = symptom_onset_date - 101,
      # determine the survey perios to which each exposure period belongs
      survey_period = case_when(
        exposure_start_date <= survey$winter_end_date &
          exposure_end_date >= survey$winter_start_date ~ "winter",
        exposure_start_date <= survey$summer_end_date &
          exposure_end_date >= survey$summer_start_date ~ "summer",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(
      type_of_contact == "Resident" | !is.na(exposure_MB2011),
      !is.na(symptom_onset_date)
    ) %>%
    mutate(
      MB2011 = coalesce(exposure_MB2011, MB2011) 
    ) %>%
    select(
      MB2011,
      symptom_onset_date,
      exposure_start_date,
      exposure_end_date,
      survey_period,
      likely_exposure
    )

}
