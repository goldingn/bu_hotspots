#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_survey_periods
#' @return
#' @author Nick Golding
#' @export
plot_cases_by_period <- function(cases) {

  cases %>%
    arrange(
      symptom_onset_date
    ) %>%
    mutate(
      case = factor(row_number()),
      .before = everything()
    ) %>%
    ggplot(
      aes(
        x = symptom_onset_date,
        xmin = exposure_start_date,
        xmax = exposure_end_date,
        y = case,
        colour = period
      )
    ) +
    geom_point() +
    geom_errorbarh() +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    xlab("date")
}
