#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases
#' @param meshblocks
#' @return
#' @author Nick Golding
#' @export
prep_meshblock_incidence_Geelong <- function(cases, meshblocks) {

  # compute counts by survey period in meshblocks with any cases
  cases_grouped <- cases %>%
    mutate(
      cases = 1
    ) %>%
    group_by(
      period,
      MB2011
    ) %>%
    summarise(
      cases = sum(cases),
      .groups = "drop"
    )
  
  # join cases to meshblocks to obtain incidence data
  expand.grid(
    period = unique(cases_grouped$period),
    MB_CODE11 = unique(meshblocks$MB_CODE11)
  ) %>%
    left_join(
      meshblocks,
      by = "MB_CODE11"
    ) %>%
    st_as_sf() %>%
    left_join(
      cases_grouped,
      by = c(
        "MB_CODE11" = "MB2011",
        "period" = "period"
      )
    ) %>%
    mutate(
      cases = replace_na(cases, 0)
    ) %>%
    dplyr::select(
      period,
      meshblock = MB_CODE11,
      cases,
      area_sqm = ALBERS_SQM,
      pop = Population,
      geometry
    ) %>%
    # keep only those with some population, and area less than 500m x 500m (to
    # retain spatial precision)
    filter(
      pop > 0,
      pop <= 200,
      area_sqm <= 500 ^ 2
    ) %>%
    mutate(
      incidence = cases / pop
    )
  
}
