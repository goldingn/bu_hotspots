#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meshblock_incidence_survey_periods_blocked
#' @return
#' @author Nick Golding
#' @export
plot_blocked_incidence <- function(meshblock_incidence_survey_periods_blocked) {
  
  # plot incidence within each of these
  meshblock_incidence_survey_periods_blocked %>%
    group_by(
      block
    ) %>%
    mutate(
      incidence = sum(cases) / sum(pop)
    ) %>%
    ungroup() %>%
    ggplot(
      aes(
        fill = incidence,
        colour = cases > 0
      )
    ) +
    geom_sf(
      pch = 21
    ) +
    coord_sf() +
    scale_fill_distiller(direction = 1) +
    scale_colour_manual(values = c("transparent", "black")) +
    scale_shape_discrete() +
    theme_minimal() 

}