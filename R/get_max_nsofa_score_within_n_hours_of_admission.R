#' Calculate maximum nsofa score within a time range
#'
#' @param nsofa_scores a dataframe contianing q1hr nsofa scores
#' @param min_hour lower timepoint
#' @param max_hour upper timepoint
#'
#' @return A data frame with the max nosfa score within a given time range
#'
#' @export
get_max_nosfa_score_within_n_hours_of_admission <- function(nsofa_scores, min_hour, max_hour) {
  max_score <- nsofa_scores %>%
    dplyr::group_by(.data$child_mrn_uf) %>%
    dplyr::arrange(.data$q1hr) %>%
    # every row represents an hour
    dplyr::mutate(hour = 1:dplyr::n()) %>%
    dplyr::filter(dplyr::between(.data$hour, min_hour, max_hour)) %>%
    dplyr::summarise(
      nsofa_score = max(.data$nsofa_score),
      number_hours_in_encounter = max(.data$hour),
      dischg_disposition = unique(.data$dischg_disposition)
    )

  return(max_score)
}
