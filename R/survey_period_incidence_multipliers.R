#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
# compute multipliers between annual incidence and incidence (of infection) over
# the two survey periods
survey_period_incidence_multipliers <- function() {
  
  define_survey_periods() %>%
    mutate(
      summer_range = as.numeric(summer_end_date - summer_start_date),
      winter_range = as.numeric(winter_end_date - winter_start_date),
      multiplier_summer = summer_range / 365.25,
      multiplier_winter = winter_range / 365.25,
    ) %>%
    select(
      starts_with("multiplier")
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "period",
      values_to = "multiplier",
      names_prefix = "multiplier_"
    )
  
}
