#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author Nick Golding
#' @export
get_mb_2011_2016_lookup <- function() {

  # convert to 2011 centroids
  cg_file <- "data/CG_VIC_MB_2011_VIC_MB_2016.xls"
  mb_lookup_1 <- readxl::read_excel(
    cg_file,
    sheet = "Table 3a",
    skip = 5
  ) %>%
    dplyr::slice(-1)
  
  mb_lookup_2 <- readxl::read_excel(
    cg_file,
    sheet = "Table 3b",
    skip = 5
  ) %>%
    dplyr::slice(-1)
  
  bind_rows(
    mb_lookup_1,
    mb_lookup_2
  ) %>%
    select(
      MB_CODE11 = `MB_CODE_2011...1`,
      MB_CODE_2016 = `MB_CODE_2016...3`,
      weight = RATIO
    ) %>%
    mutate(
      across(
        starts_with("MB"),
        as.character
      )
    )

}
