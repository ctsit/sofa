#' Calculate neurologic score
#'
#' @description Ingests the glasgow data and calculates the neurologic score.
#'
#' @details
#' \itemize{
#'   \item When there are both glasgow_coma_adult_score and glasgow_comma_peds_score recorded for a
#'   given patient on the same encounter the lower score is used to avoid underestimating the severity
#'   \item neurologic scores are carried forward until a new neurologic score is recorded
#'   \item When multiple neurologic scores recorded within an hour the lowest neurologic score is used
#' }
#'
#' @param read_glasgow the raw glasgow csv file obtained from the IDR
#'
#' @return A data frame with the neurologic scores
#'
#' @export
get_psofa_neurologic <- function(read_glasgow) {
  neurologic <- read_glasgow %>%
    dplyr::rename_all(~ stringr::str_remove(., "glasgow_coma_")) %>%
    dplyr::mutate(gcs_datetime = lubridate::mdy_hm(.data$datetime)) %>%
    dplyr::arrange(.data$child_mrn_uf, .data$gcs_datetime) %>%
    # where there are both adult_score and peds_score recorded for the same patient on the same encounter, AND
    # the scores are discrepant, we use the lower one to avoid under estimating the severity.
    dplyr::mutate(
      q1hr = lubridate::floor_date(.data$gcs_datetime, "1 hour"),
      total_score = pmin(.data$adult_score, .data$peds_score, na.rm = TRUE),
      neurologic_score = dplyr::case_when(
        .data$total_score < 6 ~ 4,
        dplyr::between(.data$total_score, 6, 9) ~ 3,
        dplyr::between(.data$total_score, 10, 12) ~ 2,
        dplyr::between(.data$total_score, 13, 14) ~ 1,
        .data$total_score == 15 ~ 0
      )
    ) %>%
    # fill in NA values with the last calculated nuerologic score
    dplyr::group_by(.data$child_mrn_uf) %>%
    tidyr::fill(.data$neurologic_score, .direction = "down") %>%
    # if multiple scores are recorded per subject within 1 hr take the lowest neurologic score
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    dplyr::slice_min(.data$neurologic_score, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$neurologic_score)

  return(neurologic)
}
