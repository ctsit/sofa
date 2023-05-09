#' Set respiratory status to TRUE/FALSE
#'
#' @description Sets the respiratory status based on the oxygenation or vent settings device present
#' \itemize{
#'   \item Instances of mean airway pressure or vent map under vent settings flowsheet indicate on respiratory support
#'   \item Instances of bipap or cpap under oxygenation flowsheet indicate on respiratory support
#'   \item Devices in categorized_respiratory_devices under oxygenation flowsheet indicate not on respiratory support
#'  }
#'
#' @param read_flowsheets the raw flowsheets csv file obtained from the IDR
#' @param categorized_respiratory_devices an excel file containing the categorized respiratory devices
#'
#' @return A dataframe containing the respiratory status
#'
#' @export
get_psofa_respiratory_status <- function(read_flowsheets, categorized_respiratory_devices) {
  respiratory_support_status <- read_flowsheets %>%
    dplyr::mutate(
      on_respiratory_support = dplyr::case_when(
        .data$flowsheet_group == "vent settings" &
          .data$flo_meas_name %in%  c("r rt (osc) mean airway pressure", "r vent map") ~ TRUE,
        .data$flowsheet_group == "oxygenation" &
          .data$meas_value %in% c("bipap", "cpap") ~ TRUE,
        .data$flowsheet_group == "oxygenation" &
          .data$meas_value %in% categorized_respiratory_devices$meas_value ~ FALSE
      )
    ) %>%
    dplyr::filter(!is.na(.data$on_respiratory_support)) %>%
    dplyr::mutate(q1hr = lubridate::floor_date(.data$recorded_time, "1hour")) %>%
    # prioritize TRUE if there are multiple resp support status in an hour
    dplyr::arrange(.data$child_mrn_uf, dplyr::desc(.data$on_respiratory_support)) %>%
    dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = TRUE)

  return(respiratory_support_status)
}


#' PaO2/FiO2 Respiratory Score
#'
#' @description Calculates the respiratory score based on the PaO2/FiO2 ratio. The PaO2/FiO2 ratio is only
#' calculated when the PaO2 and FiO2 values are recorded simultaneously
#'
#' @param read_child_labs the raw child labs csv file obtained from the IDR
#' @param transformed_child_encounter a df returned by \code{\link{get_psofa_child_encounter}}
#' @param respiratory_support_status a df returned by \code{\link{get_psofa_respiratory_status}}
#'
#' @return A dataframe containing the respiratory scores based on the paO2/fiO2 ratio
#'
#' @export
get_psofa_pao2_fio2_resp_score <- function(read_child_labs, transformed_child_encounter, respiratory_support_status) {
  pao2_fio2_resp_score <- read_child_labs %>%
    dplyr::filter(
      .data$child_mrn_uf %in% transformed_child_encounter$child_mrn_uf &
        .data$proc_desc %in% c("blood gas, arterial", "blood gas, arterial (poc)") &
        .data$lab_name %in% c(
          "po2 arterial",
          "po2 arterial (point of  care)",
          "po2 arterial (point of care)",
          "fraction inspired oxygen (abg)",
          "fraction inspired oxygen"
        )
    ) %>%
    dplyr::mutate(
      lab_result = as.numeric(.data$lab_result),
      lab_name = dplyr::case_when(
        .data$lab_name %in% c("fraction inspired oxygen (abg)", "fraction inspired oxygen") ~ "fio2",
        TRUE ~ "pao2"
      ),
      lab_result = dplyr::case_when(
        .data$lab_name == "fio2" & .data$lab_result >= 15 ~ lab_result / 100,
        .data$lab_name == "fio2" &
          .data$lab_result < 15 & .data$lab_result > 1 ~ NA_real_,
        .data$lab_name == "fio2" & .data$lab_result == 0 ~ NA_real_,
        TRUE ~ .data$lab_result
      )
    ) %>%
    dplyr::add_count(.data$child_mrn_uf, .data$inferred_specimen_datetime) %>%
    # pair pao2 to fio2 at exact time
    dplyr::filter(.data$n == 2) %>%
    dplyr::select(.data$child_mrn_uf,
           .data$inferred_specimen_datetime,
           .data$lab_name,
           .data$lab_result) %>%
    tibble::rownames_to_column(.) %>%
    tidyr::pivot_wider(names_from = .data$lab_name, values_from = .data$lab_result) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$inferred_specimen_datetime) %>%
    tidyr::fill(c(.data$fio2, .data$pao2), .direction = "downup") %>%
    dplyr::select(-.data$rowname) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    # it must be a paired value
    dplyr::filter(!is.na(.data$fio2) & !is.na(.data$pao2)) %>%
    dplyr::mutate(
      ratio = round(.data$pao2 / .data$fio2),
      q1hr = lubridate::floor_date(.data$inferred_specimen_datetime, "1hour"),
           source = "pao2_fio2") %>%
    # use the lowest ratio within an hour
    dplyr::arrange(.data$child_mrn_uf, .data$q1hr, .data$ratio) %>%
    dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = TRUE) %>%
    dplyr::left_join(
      respiratory_support_status %>%
        dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$on_respiratory_support),
      by = c("child_mrn_uf", "q1hr")
    ) %>%
    dplyr::mutate(
      resp_score = dplyr::case_when(
        .data$ratio >= 400 ~ 0,
        dplyr::between(.data$ratio, 300, 399) ~ 1,
        dplyr::between(.data$ratio, 200, 299) ~ 2,
        .data$on_respiratory_support & between(.data$ratio, 100, 199) ~ 3,
        .data$on_respiratory_support & .data$ratio < 100 ~ 4,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(.data$resp_score)) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, pao2_fio2_resp_score = .data$resp_score)

  return(pao2_fio2_resp_score)
}

#' Nasal Cannula Flow Rates
#'
#' @description Creates a substitute fio2 based on nasal flow rate
#' \itemize{
#'   \item low flow states (≤2L) will likely be using variable oxygen concentrations from the
#'    wall but will most likely have low relative oxygen delivery because of low flow rates.
#'    Because the relative FiO2 delivered is low no matter what FiO2 value is charted,
#'    the resultant SpO2/FiO2 ratios would be very high and preclude a resp score of > 0.
#'    So instances where the flow rate is ≤2L/min can be designated as a resp score of “0”
#'    in that hour without the need to calculate the SpO2/FiO2 ratio.
#'
#'  \item for mid-flow states of nasal cannula and high-flow nasal cannula (>2L and <6L) there
#'  are two reasonable options. One option is that if there is an FiO2 charted within the hour
#'  with these flow rates, that FiO2 is used for SpO2/FiO2 calculations.
#'  If an FiO2 is not charted in that hour we assume it is 1.0 for calculations.
#'
#'  \item sets the respiratory status to false at all timepoints
#'
#' }
#'
#' @param read_flowsheets the raw flowsheet csv file obtained from the IDR
#'
#' @return A dataframe containing the substitute FiO2 values
#'
#' @export
get_psofa_nasal_state <- function(read_flowsheets) {

  flow_rate_value <- read_flowsheets %>%
    dplyr::filter(
      .data$flowsheet_group == 'oxygenation' &
        .data$flo_meas_name == "r oxygen flow rate" &
        !is.na(.data$meas_value)
    ) %>%
    dplyr::select(.data$child_mrn_uf, .data$recorded_time, flow_value = .data$meas_value, .data$units) %>%
    dplyr::arrange(.data$child_mrn_uf, .data$recorded_time)

  nasal_flow_rate_state <- read_flowsheets %>%
    dplyr::filter(
      .data$flowsheet_group == "oxygenation" &
        .data$meas_value %in% c("nasal cannula", "high flow nasal cannula")
    ) %>%
    dplyr::select(.data$child_mrn_uf, .data$recorded_time, .data$disp_name, .data$meas_value) %>%
    dplyr::arrange(.data$child_mrn_uf, .data$recorded_time) %>%
    dplyr::inner_join(flow_rate_value, by = c("child_mrn_uf", "recorded_time")) %>%
    dplyr::mutate_at("flow_value", as.numeric) %>%
    dplyr::filter(!is.na(.data$flow_value)) %>%
    dplyr::mutate(
      nasal_fio2 = dplyr::case_when(
        .data$meas_value %in% c("nasal cannula", "high flow nasal cannula") &
          .data$flow_value <= 2 ~ 0.001,
        .data$meas_value %in% c("nasal cannula", "high flow nasal cannula") &
          .data$flow_value > 2 ~ 1,
        TRUE ~ NA_real_
      ),
      q1hr = lubridate::floor_date(.data$recorded_time, "1 hour"),
      on_respiratory_support = FALSE
    ) %>%
    # When multiple flow values are recorded within an hour take the max
    dplyr::arrange(.data$child_mrn_uf, .data$q1hr, dplyr:: desc(.data$flow_value)) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$nasal_fio2, .data$on_respiratory_support) %>%
    dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = TRUE)

  return(nasal_flow_rate_state)

}


#' Extract FiO2 Value
#'
#' @description Extracts the FiO2 value from the oxygenation and vent settings flowsheets
#' \itemize{
#'   \item  When there are simultaneously-recorded FiO2 in both the oxygenation and vent settings
#'   flowsheet within an hour chose the oxygenation flowsheet value.
#'
#'  \item When there are more than two FiO2 values within an hour choose the highest value
#'   regardless of if that value comes from  oxygenation or vent settings flowsheet.
#' }
#'
#' @param categorized_respiratory_devices the respiratory support devices
#' @param read_flowsheets the raw flowsheet csv file obtained from the IDR
#' @param nasal_flow_rate_state a df returned by \code{\link{get_psofa_nasal_state}}
#'
#' @return A dataframe containing the FiO2 values
#'
#' @export
get_psofa_fio2_value <- function(categorized_respiratory_devices, read_flowsheets, nasal_flow_rate_state) {

  # exclude nasal devices from this as they stop devices
  fio2_continuation_devices <- categorized_respiratory_devices %>%
    dplyr::filter(
      .data$fio2_to_use == 'charted FiO2 value' &
        !stringr::str_detect(.data$meas_value, "nasal")
    )

  fio2_value <- read_flowsheets %>%
    dplyr::filter(
      .data$flowsheet_group %in% c('oxygenation', 'vent settings') &
        stringr::str_detect(.data$disp_name, "fio2") |
        .data$flowsheet_group == "oxygenation" &
        .data$meas_value %in% fio2_continuation_devices$meas_value
    ) %>%
    dplyr::mutate_at("meas_value", as.numeric) %>%
    dplyr::arrange(.data$child_mrn_uf, .data$recorded_time) %>%
    dplyr::mutate(q1hr = lubridate::floor_date(.data$recorded_time, "1hour")) %>%
    dplyr::mutate(
      lab_result = dplyr::case_when(
        dplyr::between(.data$meas_value, 20, 100) ~ .data$meas_value / 100,
        .data$meas_value < 20 & .data$meas_value > 1 ~ NA_real_,
        .data$meas_value == 0 | .data$meas_value > 100 ~ NA_real_,
        TRUE ~ .data$meas_value
      )
    ) %>%
    dplyr::arrange(.data$meas_value) %>%
    # count number of fio2 scores recorded within an hour
    dplyr::add_count(.data$child_mrn_uf, .data$q1hr, name = "number_of_scores") %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    dplyr::mutate(
      priority = dplyr::case_when(
        .data$number_of_scores == 1 ~ 1,
        # When there are simultaneously-recorded FiO2 in both the “oxygenation” and “vent settings”
        # flowsheet  within an hour chose the “oxygenation” flowsheet value.
        .data$number_of_scores == 2 &
          .data$flowsheet_group == "Oxygenation" &
          !is.na(.data$lab_result) ~ 1,
        # When there are more than two FiO2 values within the same hour choose the highest value
        # regardless of if that value comes from  "oxygenation" or "vent settings" flowsheet group.
        .data$number_of_scores > 2 &
          .data$lab_result == max(.data$lab_result) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    # if priority is 0 and there are multiple scores within an hour use the highest fio2.
    dplyr::arrange(
      .data$child_mrn_uf,
      .data$q1hr,
      dplyr::desc(.data$priority),
      dplyr::desc(.data$lab_result)
    ) %>%
    dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = T) %>%
    dplyr::select(.data$child_mrn_uf,
                  .data$recorded_time,
                  .data$q1hr,
                  value = .data$lab_result)

  complete_fio2_value <- fio2_value %>%
    dplyr::full_join(nasal_flow_rate_state, by = c("child_mrn_uf", "q1hr")) %>%
    dplyr::arrange(.data$child_mrn_uf, .data$q1hr) %>%
    dplyr::mutate(fio2 = dplyr::coalesce(.data$value, .data$nasal_fio2)) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$fio2)

  return(complete_fio2_value)

}

#' Extract SpO2 Value
#'
#' @description Extracts the SpO2 value from the vitals flowsheets
#' \itemize{
#'   \item Priority: SpO2, SpO2 #3, then SpO2 #2
#'
#'  \item when there are multiple values for any given SpO2 within an hour
#'  choose the lowest value
#' }
#'
#' @param read_flowsheets the raw flowsheet csv file obtained from the IDR
#'
#' @return A dataframe containing the SpO2 values
#'
#' @export
get_psofa_spo2_value <- function(read_flowsheets) {
  spo2_value <- read_flowsheets %>%
    dplyr::filter(.data$flowsheet_group == 'vitals' &
                    stringr::str_detect(.data$disp_name, "spo2")) %>%
    dplyr::mutate(q1hr = lubridate::floor_date(.data$recorded_time, "1hour")) %>%
    dplyr::select(.data$child_mrn_uf, .data$recorded_time, .data$q1hr, .data$meas_value, .data$disp_name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(meas_value = as.numeric(.data$meas_value)) %>%
    # Choose an SpO2 value following this order: SpO2, SpO2 #3, then SpO2 #2
    dplyr::mutate(
      score_priority = dplyr::case_when(
        .data$disp_name == "spo2" ~ 1,
        .data$disp_name == "spo2 #3 (or spo2po)" ~ 2,
        .data$disp_name == "spo2 #2 (or spo2pr)" ~ 3
      )
    ) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    # when there are multiple values for any given SpO within an hour
    # choose the lowest meas_value
    dplyr::arrange(.data$q1hr, .data$score_priority, .data$meas_value) %>%
    dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = T) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, spo2 = .data$meas_value)

  return(spo2_value)
}

#' Calculate respiratory score
#'
#' @param expanded_child_encounter a df returned by \code{\link{get_psofa_child_encounter}}
#' @param pao2_fio2_resp_score a df returned by \code{\link{get_psofa_pao2_fio2_resp_score}}
#' @param complete_fio2_value a df returned by \code{\link{get_psofa_fio2_value}}
#' @param spo2_value a df returned by \code{\link{get_psofa_spo2_value}}
#' @param respiratory_support_status a df returned by \code{\link{get_psofa_respiratory_status}}
#' @param nasal_flow_rate_state a df returned by \code{\link{get_psofa_nasal_state}}
#'
#' @return A dataframe containing the respiratory scores
#'
#' @export
get_psofa_respiratory <-
  function(expanded_child_encounter,
           pao2_fio2_resp_score,
           complete_fio2_value,
           spo2_value,
           respiratory_support_status,
           nasal_flow_rate_state) {

    respiratory <- expanded_child_encounter %>%
      dplyr::left_join(pao2_fio2_resp_score, by = c("child_mrn_uf", "q1hr")) %>%
      dplyr::left_join(complete_fio2_value, by = c("child_mrn_uf", "q1hr")) %>%
      dplyr::left_join(spo2_value, by = c("child_mrn_uf", "q1hr")) %>%
      dplyr::group_by(.data$child_mrn_uf, .data$encounter) %>%
      dplyr::arrange(.data$child_mrn_uf, .data$encounter, .data$q1hr) %>%
      # fill NAs with the last recorded value
      tidyr::fill(c(.data$fio2, .data$spo2), .direction = "down") %>%
      dplyr::mutate(ratio = round(.data$spo2 / .data$fio2)) %>%
      dplyr::left_join(
        respiratory_support_status %>%
          dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$on_respiratory_support),
        by = c("child_mrn_uf", "q1hr")
      ) %>%
      # tag all nasal resp_scores in a q1hr as not resp support
      dplyr::left_join(
        nasal_flow_rate_state %>%
          dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$on_respiratory_support),
        by = c("child_mrn_uf", "q1hr")
      ) %>%
      dplyr::mutate(on_respiratory_support = dplyr::coalesce(.data$on_respiratory_support.x, .data$on_respiratory_support.y)) %>%
      tidyr::fill(.data$on_respiratory_support, .direction = "down") %>%
      dplyr::mutate(
        spo2_fio2_resp_score = dplyr::case_when(
          .data$on_respiratory_support & .data$ratio < 148 ~ 4,
          .data$on_respiratory_support & dplyr::between(.data$ratio, 148, 220) ~ 3,
          .data$ratio < 264 ~ 2,
          dplyr::between(.data$ratio, 264, 291) ~ 1,
          .data$ratio >= 292 ~ 0,
          TRUE ~ NA_real_
        ),
        respiratory_score = dplyr::coalesce(.data$pao2_fio2_resp_score, .data$spo2_fio2_resp_score)
      ) %>%
      dplyr::mutate_at("respiratory_score", ~ replace(., is.na(.), 0)) %>%
      dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
      #  Prevents multiple respiratory scores from being created for subjects that
      # were tagged as having multiple encounters when they simply moved between units.
      dplyr::slice_max(.data$respiratory_score, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$respiratory_score, .data$on_respiratory_support)

    return(respiratory)
}
