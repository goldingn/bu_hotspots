#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meshblock_incidence_survey_periods_blocked
#' @param which
#' @return
#' @author Nick Golding
#' @export
split_data <- function(blocked, which = c("train", "test")) {
  
  which <- match.arg(which)
  
  blocks <- unique(blocked$block)
  
  splits <- list()
  
  # split into three different training sets, and corresponding test sets
  for (this_block in blocks) {
    
    # this_block is used for testing, but not training
    if (which == "train") {
      
      splits[[this_block]] <- blocked %>%
        filter(
          block != this_block
        )
      
    } else {
      
      splits[[this_block]] <- blocked %>%
        filter(
          block == this_block
        )
      
    }
    
  }
  
  names(splits) <- blocks
  
  splits
  
}
