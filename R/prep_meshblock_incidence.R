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
prep_meshblock_incidence <- function(cases, meshblocks) {

  # compute counts in meshblocks with any cases
  cases_grouped <- cases %>%
    mutate(
      cases = 1
    ) %>%
    group_by(
      MB2011
    ) %>%
    summarise(
      cases = sum(cases)
    )
  
  # join cases to meshblocks to obtain incidence data
  meshblocks %>%
    left_join(
      cases_grouped,
      by = c("MB_CODE11" = "MB2011")
    ) %>%
    mutate(
      cases = replace_na(cases, 0)
    ) %>%
    select(
      meshblock = MB_CODE11,
      cases,
      area_sqm = ALBERS_SQM,
      pop = Population,
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
