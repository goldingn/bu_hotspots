#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases
#' @return
#' @author Nick Golding
#' @export
assign_seasons_Geelong <- function(cases) {

  # assign to financial years, based on the estimated infection date
  cases %>%
    mutate(
      exposure_estimate = symptom_onset_date - 143,
      quarter = lubridate::quarter(
        exposure_estimate,
        with_year = TRUE,
        fiscal_start = 7
      ),
      period = stringr::str_sub(quarter, 1, 4),
      period = factor(period),
      .before = everything()
    ) %>%
    dplyr::select(
      -exposure_estimate,
      -quarter
    )

}
