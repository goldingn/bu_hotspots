#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases
#' @return
#' @author Nick Golding
#' @export
assign_survey_periods_Geelong <- function(cases) {
  
  # starts and ends of possum scat surveys
  survey_Geelong <- define_survey_periods_Geelong()
  
  cases %>%
    mutate(
      # determine the survey periods to which each exposure period belongs
      period = case_when(
        exposure_start_date <= survey_Geelong$summer_end_date &
          exposure_end_date >= survey_Geelong$summer_start_date ~ "summer",
        exposure_start_date <= survey_Geelong$winter_end_date &
          exposure_end_date >= survey_Geelong$winter_start_date ~ "winter",
        TRUE ~ "other"
      ),
      .before = everything()
    ) %>%
    filter(
      period %in% c("summer", "winter")
    )
  
}
