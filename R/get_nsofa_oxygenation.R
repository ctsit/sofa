#' Calculates oxygenation score
#'
#' @description Sets the respiratory status based on the oxygenation or vent settings device present
#'
#' @param read_flowsheets the raw flowsheets csv file obtained from the IDR
#' @param respiratory_devices an excel file containing the categorized respiratory devices
#' @param child_encounter a df returned by \code{\link{get_nsofa_child_encounter}}
#'
#' @return A dataframe containing the oxygenation score
#'
#' @export
get_nsofa_oxygenation <- function(read_flowsheets, respiratory_devices, child_encounter) {
  flowsheets <- read_flowsheets %>%
    dplyr::filter(.data$child_mrn_uf %in% child_encounter$child_mrn_uf) %>%
    dplyr::group_by(.data$child_mrn_uf) %>%
    dplyr::mutate(q1hr = lubridate::floor_date(.data$recorded_time, "1 hour"))

  intubated_yes_no <- flowsheets %>%
    dplyr::filter(.data$flowsheet_group == "Oxygenation" &
                    .data$disp_name == "Respiratory Device") %>%
    dplyr::inner_join(respiratory_devices, by = "meas_value") %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    # choose last recorded value within q1hr
    dplyr::filter(.data$recorded_time == max(.data$recorded_time)) %>%
    dplyr::arrange(.data$child_mrn_uf,
                   .data$q1hr,
                   dplyr::desc(.data$intubated)) %>%
    dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = T) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$child_mrn_uf,
                  .data$q1hr,
                  .data$meas_value,
                  .data$intubated)

  # set intubated to yes or no at every hour during an encounter
  intubated_yes_no <- child_encounter %>%
    dplyr::left_join(intubated_yes_no, by = c("child_mrn_uf", "q1hr")) %>%
    dplyr::group_by(.data$child_mrn_uf) %>%
    tidyr::fill(c(.data$meas_value, .data$intubated), .direction = "down")

  fio2_score <- flowsheets %>%
    dplyr::filter(
      .data$flowsheet_group %in% c('Oxygenation', 'Vent Settings') &
        stringr::str_detect(.data$disp_name, "FiO2")
    ) %>%
    # identify the timepoints at which a subject was intubated
    dplyr::inner_join(
      intubated_yes_no %>%
        dplyr::filter(.data$intubated == 'yes') %>%
        dplyr::select(.data$child_mrn_uf, .data$q1hr),
      by = c("child_mrn_uf", "q1hr")
    ) %>%
    dplyr::select(
      .data$child_mrn_uf,
      .data$recorded_time,
      .data$q1hr,
      .data$flowsheet_group,
      .data$meas_value,
      .data$disp_name
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$recorded_time) %>%
    # count number of fio2 scores recorded within an hour
    dplyr::add_count(.data$child_mrn_uf, .data$q1hr, name = "number_of_scores") %>%
    dplyr::mutate(meas_value = as.numeric(.data$meas_value)) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    dplyr::mutate(na_meas_value = dplyr::if_else(is.na(.data$meas_value), 0, 1)) %>%
    dplyr::mutate(
      priority = dplyr::case_when(
        .data$number_of_scores == 1 ~ 1,
        # When there are simultaneously-recorded FiO2 in both the “oxygenation” and “vent settings”
        # flowsheet  within an hour chose the “oxygenation” flowsheet value.
        .data$number_of_scores == 2 &
          .data$flowsheet_group == "Oxygenation" &
          !is.na(.data$meas_value) ~ 1,
        # When there are more than two FiO2 values within the same hour choose the highest value
        # regardless of if that value comes from  "oxygenation" or "vent settings" flowsheet group.
        .data$number_of_scores > 2 &
          .data$meas_value == suppressWarnings(max(.data$meas_value, na.rm = TRUE)) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::arrange(.data$q1hr,
                   dplyr::desc(.data$na_meas_value),
                   dplyr::desc(.data$priority)) %>%
    dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = T) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, value = .data$meas_value) %>%
    dplyr::mutate(score_name = "fio2")

  spo2_score <- flowsheets %>%
    dplyr::filter(.data$flowsheet_group == "Vitals" &
                    stringr::str_detect(.data$disp_name, "SpO2")) %>%
    # identify the timepoints at which a subject was intubated
    dplyr::inner_join(
      intubated_yes_no %>%
        dplyr::filter(.data$intubated == 'yes') %>%
        dplyr::select(.data$child_mrn_uf, .data$q1hr),
      by = c("child_mrn_uf", "q1hr")
    ) %>%
    dplyr::select(
      .data$child_mrn_uf,
      .data$recorded_time,
      .data$q1hr,
      .data$flowsheet_group,
      .data$meas_value,
      .data$disp_name
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(meas_value = as.numeric(.data$meas_value)) %>%
    # Choose an SpO2 value following this order: SpO2, SpO2 #3, then SpO2 #2
    dplyr::mutate(
      score_priority = dplyr::case_when(
        .data$disp_name == "SpO2" ~ 1,
        .data$disp_name == "SpO2 #3 (or SpO2po)" ~ 2,
        .data$disp_name == "SpO2 #2 (or SpO2pr)" ~ 3
      )
    ) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    # when there are multiple values for any given SpO within an hour
    # choose the lowest meas_value
    dplyr::arrange(.data$q1hr, .data$score_priority, .data$meas_value) %>%
    dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = T) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, value = .data$meas_value) %>%
    dplyr::mutate(score_name = "spo2")

  intubated_yes <- fio2_score %>%
    dplyr::bind_rows(spo2_score) %>%
    dplyr::group_by(.data$child_mrn_uf) %>%
    # convert fio2 and spo2 to columns
    tidyr::pivot_wider(names_from = .data$score_name,
                       values_from = .data$value) %>%
    dplyr::arrange(.data$q1hr) %>%
    # fill NAs with the last recorded value
    tidyr::fill(c(.data$fio2, .data$spo2), .direction = "down") %>%
    dplyr::mutate(oxygenation_ratio = .data$spo2 / .data$fio2) %>%
    dplyr::mutate(
      oxygenation = dplyr::case_when(
        .data$oxygenation_ratio >= 3 ~ 0,
        .data$oxygenation_ratio >= 2 ~ 2,
        .data$oxygenation_ratio >= 1.5 ~ 4,
        .data$oxygenation_ratio >= 1 ~ 6,
        .data$oxygenation_ratio < 1 ~ 8,
        TRUE ~ NA_real_
      )
    )

  intubated_no <- intubated_yes_no %>%
    dplyr::filter(.data$intubated == 'no') %>%
    dplyr::mutate(oxygenation = 0)

  oxygenation <- intubated_yes %>%
    dplyr::bind_rows(intubated_no) %>%
    dplyr::select(.data$child_mrn_uf,
                  .data$q1hr,
                  .data$fio2,
                  .data$spo2,
                  .data$oxygenation) %>%
    dplyr::arrange(.data$child_mrn_uf, .data$q1hr)

  return(oxygenation)
}
