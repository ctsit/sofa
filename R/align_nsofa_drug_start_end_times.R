#' Aligns the start and end times for drun dosages
#' @description Ensures that the end time dosage occurs after the start time dosage and converts the dosage time to binary
#'              1 = yes, 0 = no to indicate if the subject was on that drug during a q1hr
#'
#'\itemize{
#'   \item Ensures that the end time dosage occurs after the start time dosage
#'   \item Converts the dosage time to 1 = yes, 0 = no to indicate if the subject was on that drug during a q1hr
#'   \item Rounds the dosage time down to the nearest hour
#'   }
#'
#' @param df a dataste containing drug dosages
#'
#' @return A data frame indicating if a subjects was on a given drug during a given hour
#'
#' @export
#'
align_nsofa_drug_start_end_times <- function(df) {
  aligned_times <- df %>%
    dplyr::filter(.data$med_order_datetime < .data$med_order_end_datetime) %>%
    dplyr::select(.data$child_mrn_uf,
                  .data$med_order_desc,
                  .data$med_order_datetime,
                  .data$med_order_end_datetime) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$med_order_desc) %>%
    dplyr::mutate(
      med_order_datetime = lubridate::floor_date(.data$med_order_datetime, "1 hour"),
      floor_med_order_end_datetime = lubridate::floor_date(.data$med_order_end_datetime, "1 hour")
    ) %>%
    dplyr::distinct(
      .data$child_mrn_uf,
      .data$med_order_desc,
      .data$med_order_datetime,
      .data$med_order_end_datetime,
      .keep_all = T
    ) %>%
    dplyr::arrange(.data$med_order_desc,
                   .data$med_order_datetime,
                   dplyr::desc(.data$med_order_end_datetime)) %>%
    dplyr::distinct(.data$med_order_datetime, .keep_all = T) %>%
    dplyr::mutate(
      lag_med_order = dplyr::lag(.data$med_order_datetime),
      lag_med_order_end = dplyr::lag(.data$med_order_end_datetime)
    ) %>%
    dplyr::mutate(
      med_order_datetime = dplyr::if_else(
        .data$lag_med_order_end >= .data$med_order_datetime & !is.na(.data$lag_med_order_end),
        .data$lag_med_order_end,
        .data$med_order_datetime
      )
    ) %>%
    dplyr::arrange(.data$med_order_desc,
                   .data$med_order_datetime,
                   dplyr::desc(.data$med_order_end_datetime)) %>%
    dplyr::distinct(.data$med_order_datetime, .keep_all = T) %>%
    dplyr::filter(.data$med_order_datetime < .data$med_order_end_datetime) %>%
    dplyr::select(.data$child_mrn_uf,
                  .data$med_order_desc,
                  .data$med_order_datetime,
                  .data$med_order_end_datetime) %>%
    tidyr::pivot_longer(
      cols = c(.data$med_order_datetime, .data$med_order_end_datetime),
      names_to = "time_name",
      values_to = "q1hr"
    ) %>%
    dplyr::mutate(drug_given = dplyr::if_else(.data$time_name == "med_order_datetime", 1, 0)) %>%
    dplyr::arrange(dplyr::desc(.data$drug_given)) %>%
    dplyr::distinct(.data$q1hr, .keep_all = T) %>%
    tidyr::pivot_wider(
      id_cols = c(.data$child_mrn_uf, .data$q1hr),
      names_from = .data$med_order_desc,
      values_from = .data$drug_given
    ) %>%
    dplyr::arrange(.data$q1hr) %>%
    dplyr::group_by(.data$child_mrn_uf) %>%
    tidyr::fill(-c(.data$q1hr, .data$child_mrn_uf), .direction = "down") %>%
    dplyr::mutate(q1hr = lubridate::floor_date(.data$q1hr, "1 hour")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric, ~ replace(., is.na(.), 0))

  return(aligned_times)
}
