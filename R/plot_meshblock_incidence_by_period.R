#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meshblock_incidence_seasons
#' @return
#' @author Nick Golding
#' @export
plot_meshblock_incidence_by_period <- function(
  meshblock_incidence
) {

  meshblock_incidence %>%
    arrange(
      incidence
    ) %>%
    ggplot(
      aes(
        geometry = geometry,
        colour = incidence
      )
    ) +
    facet_wrap(
      ~period,
      ncol = 3
    ) +
    geom_sf() +
    scale_colour_distiller(
      direction = 1
    ) +
    theme_minimal()

}
