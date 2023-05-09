#' Calculate Inotropes score
#'
#' @param read_medications  the raw child medication csv file obtained from the IDR
#' @param child_encounter a dataframe returned by  \code{\link{get_nsofa_child_encounter}}
#'
#' @return A data frame with the calculated inotropes score
#'
#' @export
get_nsofa_inotropes <- function(read_medications, child_encounter) {
  inotropes <- read_medications %>%
    dplyr::filter(.data$child_mrn_uf %in% child_encounter$child_mrn_uf) %>%
    dplyr::filter(
      stringr::str_detect(
        .data$med_order_desc,
        paste0(
          "DOBUTAMINE|DOPAMINE|EPINEPHRINE|MILRINONE|",
          "NOREPINEPHRINE|VASOPRESSIN|PHENYLEPHRINE"
        )
      ) &
        .data$med_order_route == "Intravenous",
      .data$med_order_discrete_dose_unit %in% c(
        "mcg/kg/min",
        "Units/min",
        "milli-units/kg/min",
        "milli-units/kg/hr"
      )
    ) %>%
    dplyr::filter(!stringr::str_detect(.data$med_order_desc, "ANESTHESIA")) %>%
    dplyr::mutate(med_order_desc = stringr::word(.data$med_order_desc, 1)) %>%
    sofa::align_nsofa_drug_start_end_times() %>%
    dplyr::mutate(
      number_inotropic_drugs = rowSums(.[-c(1, 2)]),
      inotrope_score = dplyr::case_when(
        .data$number_inotropic_drugs == 0 ~ 0,
        .data$number_inotropic_drugs == 1 ~ 2,
        .data$number_inotropic_drugs > 1 ~  3,
        TRUE ~ NA_real_
      )
    )

  return(inotropes)
}
