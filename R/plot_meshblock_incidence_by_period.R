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
  meshblock_incidence_seasons,
  meshblocks
) {

  meshblock_incidence_seasons %>%
    arrange(
      incidence
    ) %>%
    left_join(
      meshblocks,
      by = c("meshblock" = "MB_CODE11")
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
