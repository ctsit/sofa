#' Creates q1hr timepoints within an encounter
#' @description This function ingests the raw child encounter data and performs the following:
#' \itemize{
#'   \item choose first encounter when a subject has multiple encounters
#'   \item removes rows where both the admit_datetime and dischg_datetime is NA
#'   \item Creates the q1hr time buckets
#' }
#'
#' @param read_child_encounter the raw child_encounter csv file obtained from the IDR
#'
#' @return A dataframe containing q1hr timepoints
#'
#' @export

get_nsofa_child_encounter <- function(read_child_encounter) {
  child_encounter <- read_child_encounter %>%
    # choose first encounter when a subject has multiple encounters
    dplyr::arrange(.data$child_mrn_uf, .data$admit_datetime) %>%
    dplyr::distinct(.data$child_mrn_uf, .keep_all = T) %>%
    dplyr::group_by(.data$child_mrn_uf) %>%
    dplyr::filter(!is.na(.data$admit_datetime) &
                    !is.na(.data$dischg_datetime)) %>%
    dplyr::select(
      .data$child_mrn_uf,
      .data$admit_datetime,
      .data$child_birth_date,
      .data$dischg_disposition,
      .data$dischg_datetime
    ) %>%
    # create q1hr timepoints
    tidyr::expand(
      .data$child_birth_date,
      .data$dischg_disposition,
      .data$admit_datetime,
      .data$dischg_datetime,
      q1hr = seq(
        lubridate::floor_date(.data$admit_datetime, "1 hour"),
        lubridate::floor_date(.data$dischg_datetime, "1 hour"),
        by = "hours"
      )
    )

  return(child_encounter)
}
