#' Calculate renal score
#' @description Ingests the raw lab data data calculates the renal score.
#'
#' @details
#' \itemize{
#'   \item Filter for labs that are named "creatine", "creatinine (point of care)",
#'         "creatinine, serum", "creatinine-plasma". Additionally, the lab result must be numeric.
#'   \item When multiple lab results are recorded within an hour the latest lab result is used.
#' }
#'
#' @param read_child_labs the raw glasgow csv file obtained from the IDR
#' @param child_dob the dob of each subjects
#'
#' @return A data frame with the calculated renal score
#'
#' @export
get_psofa_renal <- function(read_child_labs, child_dob) {
  renal <- read_child_labs %>%
    dplyr::select(
      .data$child_mrn_uf,
      .data$lab_name,
      .data$lab_result,
      .data$inferred_specimen_datetime
    ) %>%
    dplyr::filter(
      .data$lab_name %in% c(
        "creatinine",
        "creatinine (point of care)",
        "creatinine, serum",
        "creatinine-plasma"
      ) &
        !stringr::str_detect(.data$lab_result, "[[:alpha:]]")
    ) %>%
    dplyr::mutate(q1hr = lubridate::floor_date(.data$inferred_specimen_datetime, "1hour")) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    # if there are multiple values within an hour take the last recorded value
    dplyr::slice_max(.data$inferred_specimen_datetime, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(child_dob, by = "child_mrn_uf") %>%
    dplyr::mutate(
      # remove < or > sign from numbers and ensure numbers are 1 decimal place
      # to match the abstract. e,g, 2.4 -> 2.0 or 2.5 -> 3.0
      lab_result = readr::parse_number(.data$lab_result),
      age_in_months = lubridate::interval(.data$child_birth_date, lubridate::as_date(.data$q1hr)) %/% months(1),
      renal_score = dplyr::case_when(
        # <1 month
        .data$age_in_months < 1 & .data$lab_result < 0.8 ~ 0,
        .data$age_in_months < 1 & dplyr::between(.data$lab_result, 0.8, 0.99) ~ 1,
        .data$age_in_months < 1 & dplyr::between(.data$lab_result, 1.0, 1.99) ~ 2,
        .data$age_in_months < 1 & dplyr::between(.data$lab_result, 1.2, 1.59) ~ 3,
        .data$age_in_months < 1 & .data$lab_result >= 1.6 ~ 4,
        # 1-11 months
        .data$age_in_months <= 11 & .data$lab_result < 0.3 ~ 0,
        .data$age_in_months <= 11 & dplyr::between(.data$lab_result, 0.3, 0.49) ~ 1,
        .data$age_in_months <= 11 & dplyr::between(.data$lab_result, 0.5, 0.79) ~ 2,
        .data$age_in_months <= 11 & dplyr::between(.data$lab_result, 0.8, 1.19) ~ 3,
        .data$age_in_months <= 11 & .data$lab_result >= 1.2 ~ 4,
        # 12-23 months
        .data$age_in_months <= 23 & .data$lab_result < 0.4 ~ 0,
        .data$age_in_months <= 23 & dplyr::between(.data$lab_result, 0.4, 0.59) ~ 1,
        .data$age_in_months <= 23 & dplyr::between(.data$lab_result, 0.6, 1.09) ~ 2,
        .data$age_in_months <= 23 & dplyr::between(.data$lab_result, 1.1, 1.49) ~ 3,
        .data$age_in_months <= 23 & .data$lab_result >= 1.5 ~ 4,
        # 24-59 months
        .data$age_in_months <= 59 & .data$lab_result < 0.6 ~ 0,
        .data$age_in_months <= 59 & dplyr::between(.data$lab_result, 0.6, 0.89) ~ 1,
        .data$age_in_months <= 59 & dplyr::between(.data$lab_result, 0.9, 1.59) ~ 2,
        .data$age_in_months <= 59 & dplyr::between(.data$lab_result, 1.6, 2.29) ~ 3,
        .data$age_in_months <= 59 & .data$lab_result >= 2.3 ~ 4,
        # 60-143 months
        .data$age_in_months <= 143 & .data$lab_result < 0.7 ~ 0,
        .data$age_in_months <= 143 & dplyr::between(.data$lab_result, 0.7, 1.09) ~ 1,
        .data$age_in_months <= 143 & dplyr::between(.data$lab_result, 1.1, 1.79) ~ 2,
        .data$age_in_months <= 143 & dplyr::between(.data$lab_result, 1.8, 2.59) ~ 3,
        .data$age_in_months <= 143 & .data$lab_result >= 2.6 ~ 4,
        # 144-216 months
        .data$age_in_months <= 216 & .data$lab_result < 1.0 ~ 0,
        .data$age_in_months <= 216 & dplyr::between(.data$lab_result, 1.0, 1.69) ~ 1,
        .data$age_in_months <= 216 & dplyr::between(.data$lab_result, 1.7, 2.89) ~ 2,
        .data$age_in_months <= 216 & dplyr::between(.data$lab_result, 2.9, 4.19) ~ 3,
        .data$age_in_months <= 216 & .data$lab_result >= 4.2 ~ 4,
        # >216 months
        .data$age_in_months > 216 & .data$lab_result < 1.2 ~ 0,
        .data$age_in_months > 216 & dplyr::between(.data$lab_result, 1.20, 1.99) ~ 1,
        .data$age_in_months > 216 & dplyr::between(.data$lab_result, 2.0, 3.49) ~ 2,
        .data$age_in_months > 216 & dplyr::between(.data$lab_result, 3.5, 4.99) ~ 3,
        .data$age_in_months > 216 & .data$lab_result >= 5 ~ 4
      )
    ) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$renal_score)

  return(renal)
}
