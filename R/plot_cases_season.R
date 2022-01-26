#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases_seasons
#' @return
#' @author Nick Golding
#' @export
plot_cases_season <- function(cases_seasons) {

  cases_seasons %>%
    filter(
      symptom_onset_date > as.Date("2019-01-01"),
      symptom_onset_date < as.Date("2020-12-31"),
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
        colour = season
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
