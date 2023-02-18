#' Transform, filter and expand the child encounter data
#'
#' @description This function ingests the raw child encounter data and performs the following:
#' \itemize{
#'   \item Filters for all subjects less than 22 years old
#'   \item Calculates a unique encounter number for each subject based on the number of occurrences for a given mrn
#'   \item Calculates the length of stay within an encounter
#'   \item Creates the q1hr time buckets
#' }
#'
#' @param read_child_encounter the raw child_encounter csv file obtained from the IDR
#'
#' @return A list containing two data frames named transformed_child_encounter and expanded_child_encounter.
#' \itemize{
#'   \item Steps 1-3 above were executed on the raw data to create transformed_child_encounter
#'   \item Step 4 was executed on transformed_child_encounter to create expanded_child_encounter
#'}
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @export
get_psofa_child_encounter <- function(read_child_encounter) {

  transformed_child_encounter <- read_child_encounter %>%
    dplyr::arrange(.data$child_mrn_uf, .data$admit_datetime) %>%
    dplyr::mutate(age_at_admission = lubridate::time_length(
      lubridate::interval(
        .data$child_birth_date,
        lubridate::as_date(.data$admit_datetime)
      ),
      "years"
    )) %>%
    dplyr::filter(.data$age_at_admission < 22) %>%
    dplyr::group_by(.data$child_mrn_uf) %>%
    dplyr::mutate(
      encounter = dplyr::row_number(),
      age_at_admission = round(.data$age_at_admission)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(length_of_stay = as.numeric(
      difftime(
        lubridate::as_date(.data$dischg_datetime),
        lubridate::as_date(.data$admit_datetime),
        units = "days"
      )
    )) %>%
    dplyr::select(
      .data$child_mrn_uf,
      .data$encounter,
      .data$admit_datetime,
      .data$child_birth_date,
      .data$dischg_disposition,
      .data$dischg_datetime,
      .data$age_at_admission,
      .data$length_of_stay
    )

  expanded_child_encounter <- transformed_child_encounter %>%
    dplyr::group_by(.data$child_mrn_uf, .data$encounter) %>%
    tidyr::expand(
      .data$child_mrn_uf,
      .data$child_birth_date,
      .data$dischg_disposition,
      .data$admit_datetime,
      .data$dischg_datetime,
      q1hr = seq(
        lubridate::floor_date(.data$admit_datetime, "1 hour"),
        lubridate::floor_date(.data$dischg_datetime, "1 hour"),
        by = "hours"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$child_mrn_uf, .data$encounter, .data$q1hr) %>%
    dplyr::mutate(age_at_admission = lubridate::interval(.data$child_birth_date, .data$admit_datetime) %/% lubridate::years(1)) %>%
    # prevent duplicated q1hr when the dischg_datetime from the nth encounter overlaps
    # with the dischg_datetime from the nth + 1 encounter
    dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = TRUE)

  return(
    list(
      transformed_child_encounter = transformed_child_encounter,
      expanded_child_encounter = expanded_child_encounter
    )
  )
}
