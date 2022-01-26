#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases
#' @return
#' @author Nick Golding
#' @export
assign_survey_periods <- function(cases) {
  
  # starts and ends of possum scat surveys
  survey <- define_survey_periods()
  
  cases %>%
    mutate(
      # determine the survey periods to which each exposure period belongs
      period = case_when(
        exposure_start_date <= survey$summer_end_date &
          exposure_end_date >= survey$summer_start_date ~ "summer",
        exposure_start_date <= survey$winter_end_date &
          exposure_end_date >= survey$winter_start_date ~ "winter",
        TRUE ~ "other"
      ),
      .before = everything()
    ) %>%
    filter(
      period %in% c("summer", "winter")
    )
  
}
