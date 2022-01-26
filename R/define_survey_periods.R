#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
define_survey_periods <- function() {

  tibble(
    summer_start_date = "2018-12-19",
    summer_end_date = "2019-03-14",
    winter_start_date = "2019-05-28",
    winter_end_date = "2019-09-19"
  ) %>%
    mutate(
      across(
        everything(),
        as.Date
      )
    )
  
}
