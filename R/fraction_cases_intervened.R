#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param cases
#' @param pred_cases
#' @param nameme1
#' @return
#' @author Nick Golding
#' @export
# given a vector of case counts, and of predicted case counts, and a fraction of
# meshblocks that can be intervened in, calculate the fraction fo cases that
# will be found
fraction_cases_intervened <- function(cases, score, fraction_meshblocks_intervened) {
  
  number_intervened <- round(fraction_meshblocks_intervened * length(score))
  
  # rank sites
  ranking <- tibble(
    cases,
    score
  ) %>%
    arrange(
      desc(score)
    )
  
  max_score <- ranking$score[number_intervened]  
  remainder <- number_intervened - sum(ranking$score > max_score)
  number_tied <- sum(ranking$score == max_score)
  
  # all of the entries with higher prediction than max get included, all the
  # others in the next tier of tied scores get the remainder split evenly between them
  ranking <- ranking %>%
    mutate(
      intervene = case_when(
        score > max_score ~ 1,
        score == max_score ~ remainder / number_tied,
        TRUE ~ 0
      )
    )
  
  # number intervened and total number
  ranking %>%
    summarise(
      cases_intervened = sum(intervene * cases),
      total_cases = sum(cases)
    ) %>%
    mutate(
      fraction_intervened = cases_intervened / total_cases
    ) %>%
    pull(
      fraction_intervened
    )
  
}
