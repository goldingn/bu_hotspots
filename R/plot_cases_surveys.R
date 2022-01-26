#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases
#' @return
#' @author Nick Golding
#' @export
plot_cases_surveys <- function(cases) {

  surveys <- define_survey_periods()
  cases %>%
    filter(
      symptom_onset_date > as.Date("2018-11-01"),
      symptom_onset_date < as.Date("2020-06-01"),
    ) %>%
    arrange(
      symptom_onset_date
    ) %>%
    mutate(
      case = factor(row_number()),
      .before = everything()
    ) %>%
    # pivot_longer(
    #   ends_with("date"),
    #   names_to = "date_type",
    #   values_to = "date"
    # ) %>%
    ggplot(
      aes(
        x = symptom_onset_date,
        xmin = exposure_start_date,
        xmax = exposure_end_date,
        y = case,
        colour = survey_period
      )
    ) +
    geom_point() +
    geom_errorbarh() +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    xlab("date")

}
