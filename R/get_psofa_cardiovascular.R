#' Calculate cardiovascular score based on age group
#'
#' @param read_flowsheets the raw flowsheet data obtained from the IDR
#' @param child_dob the dob of each subjects
#'
#' @return A data frame with the calculated cardiovascular score by age group
#' @export
get_psofa_cv_by_age_group <- function(read_flowsheets, child_dob) {
    cv_by_age_group <- read_flowsheets %>%
      dplyr::filter(
        .data$flowsheet_group == "vitals" &
          .data$flo_meas_name %in% c("r map cuff", "r map a-line", "r map a-line 2") &
          !is.na(.data$meas_value)
      ) %>%
      dplyr::select(.data$child_mrn_uf, .data$recorded_time, .data$meas_value) %>%
      dplyr::mutate(
        q1hr = lubridate::floor_date(.data$recorded_time, "1hour"),
        meas_value = as.numeric(.data$meas_value)
      ) %>%
      dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
      # if there are multiple values within an hour take the minimum value
      dplyr::slice_min(.data$meas_value, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(child_dob, by = "child_mrn_uf") %>%
      dplyr::mutate(
        age_in_months = lubridate::interval(.data$child_birth_date, lubridate::as_date(.data$q1hr)) %/% months(1),
        age_group_cardiovascular_score = dplyr::case_when(
          .data$age_in_months < 1 & .data$meas_value >= 46 ~ 0,
          .data$age_in_months < 1 & .data$meas_value < 46 ~ 1,
          .data$age_in_months <= 11 & .data$meas_value >= 55 ~ 0,
          .data$age_in_months <= 11 & .data$meas_value < 55 ~ 1,
          .data$age_in_months <= 23 & .data$meas_value >= 60 ~ 0,
          .data$age_in_months <= 23 & .data$meas_value < 60 ~ 1,
          .data$age_in_months <= 59 & .data$meas_value >= 62 ~ 0,
          .data$age_in_months <= 59 & .data$meas_value < 62 ~ 1,
          .data$age_in_months <= 143 & .data$meas_value >= 65 ~ 0,
          .data$age_in_months <= 143 & .data$meas_value < 65 ~ 1,
          .data$age_in_months <= 216 & .data$meas_value >= 67 ~ 0,
          .data$age_in_months <= 216 & .data$meas_value < 67 ~ 1,
          .data$age_in_months > 216 & .data$meas_value >= 70 ~ 0,
          .data$age_in_months > 216 & .data$meas_value < 70 ~ 1
        )
      )

    return(cv_by_age_group)

  }

#' Calculate cardiovascular score based on vasoactive infusions
#'
#' @param read_medications the raw medications data obtained from the IDR
#' @param expanded_child_encounter a df returned by \code{\link{get_psofa_child_encounter}}
#'
#' @return A df containing the calculated cardiovascular score based on vasoactive infusions
#' @export
get_psofa_cv_by_vasoactive_infusion <- function(read_medications, expanded_child_encounter) {
  cv_by_vasoactive_infusion <- read_medications %>%
    dplyr::filter(
      .data$med_order_route == 'intravenous' &
        stringr::str_detect(
          .data$med_order_display_name,
          "dobutamine|dopamine|epinephrine|norepinephrine|milrinone|vasopressin"
        ) &
        .data$med_order_discrete_dose_unit %in% c('mcg/kg/min','milli-units/kg/min') &
        !is.na(.data$total_dose_character) &
        !.data$mar_action %in% c("missed", "mar hold", "canceled entry")
    ) %>%
    dplyr::select(
      .data$child_mrn_uf,
      .data$med_order_route,
      .data$med_order_display_name,
      .data$take_datetime,
      .data$total_dose_character,
      .data$med_order_discrete_dose_unit,
      .data$med_order_end_datetime,
      .data$mar_action
    ) %>%
    dplyr::mutate(
      q1hr = lubridate::floor_date(.data$take_datetime, "1hour"),
      med_order_end_datetime = lubridate::floor_date(.data$med_order_end_datetime, "1hour")
    ) %>%
    dplyr::group_by(.data$child_mrn_uf,
                    .data$med_order_display_name,
                    .data$q1hr) %>%
    # if the last score recorded within an hour is 0 keep that as
    # it signifies that the drug was ended.
    # Otherwise keep the highest value within an hour
    dplyr::mutate(score_priority = dplyr::case_when(
      .data$take_datetime == max(.data$take_datetime) & .data$total_dose_character == 0 ~ 3,
      .data$total_dose_character == max(.data$total_dose_character) ~ 2,
      TRUE ~ 1
    )) %>%
    dplyr::slice_max(.data$score_priority, with_ties = FALSE) %>%
    dplyr::arrange(.data$child_mrn_uf, .data$q1hr) %>%
    dplyr::filter(!is.na(.data$med_order_end_datetime)) %>%
    dplyr::select(
      .data$child_mrn_uf,
      .data$med_order_display_name,
      .data$q1hr,
      .data$total_dose_character,
    ) %>%
    # Check for volume changes within a minute for a given drug
    dplyr::mutate(med_order_display_name = stringr::word(.data$med_order_display_name, 1)) %>%
    dplyr::slice_max(.data$total_dose_character, with_ties = FALSE) %>%
    dplyr::left_join(
      expanded_child_encounter %>%
        dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$encounter, .data$dischg_disposition),
      by = c("child_mrn_uf", "q1hr")
    ) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$med_order_display_name, .data$dischg_disposition, .data$encounter) %>%
    # if the last recorded drug dose is non-zero and the subject was discharged to home
    # then set the dose to 0
    dplyr::mutate(
      total_dose_character = dplyr::if_else(
        dplyr::row_number() == dplyr::n() &
          .data$dischg_disposition %in% c("TO HOME", "TO HOMECARE"),
        0,
        .data$total_dose_character
      )
    ) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    tidyr::pivot_wider(
      names_from = .data$med_order_display_name,
      values_from = .data$total_dose_character
    ) %>%
    dplyr::arrange(.data$child_mrn_uf, .data$q1hr) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$encounter) %>%
    tidyr::fill(c(
      .data$epinephrine,
      .data$dopamine,
      .data$norepinephrine,
      .data$dobutamine,
      .data$vasopressin,
      .data$milrinone
    ),
    .direction = "down"
    ) %>%
    dplyr::mutate(
      vasoactive_cardiovascular_score = dplyr::case_when(
        .data$dopamine > 15 |
          .data$epinephrine > 0.1 | .data$norepinephrine > 0.1 ~ 4,
        .data$dopamine > 5 |
          (.data$epinephrine > 0 & .data$epinephrine <= 0.1) |
          (.data$norepinephrine > 0 & .data$norepinephrine <= 0.1) ~ 3,
        .data$dopamine > 0 | .data$dobutamine > 0 ~ 2,
        TRUE ~ NA_real_
      )) %>%
    dplyr::ungroup()

    return(cv_by_vasoactive_infusion)
  }

#' Joins the age group and vasoactive infusion cardiovascular datasets
#'
#'
#' @param cv_by_age_group a df returned by \code{\link{get_psofa_cv_by_age_group}}
#' @param cv_by_vasoactive_infusion a df returned by \code{\link{get_psofa_cv_by_vasoactive_infusion}}
#'
#' @return A data frame with the calculated cardiovascular score based on vasoactive infusions
#' @export
get_psofa_cardiovascular <- function(cv_by_age_group, cv_by_vasoactive_infusion) {
    cardiovascular <- cv_by_age_group %>%
      dplyr::full_join(cv_by_vasoactive_infusion, by = c("child_mrn_uf", "q1hr")) %>%
      dplyr::arrange(.data$child_mrn_uf, .data$q1hr) %>%
      dplyr::mutate(
        cardiovascular_score = dplyr::coalesce(
          .data$vasoactive_cardiovascular_score,
          .data$age_group_cardiovascular_score
        )
      ) %>%
      dplyr::select(
        .data$child_mrn_uf,
        .data$q1hr,
        .data$dopamine,
        .data$norepinephrine,
        .data$dobutamine,
        .data$epinephrine,
        .data$vasopressin,
        .data$milrinone,
        .data$meas_value,
        dplyr::ends_with("cardiovascular_score")
      ) %>%
      dplyr::distinct(.data$child_mrn_uf, .data$q1hr, .keep_all = TRUE)

    return(cardiovascular)
  }
