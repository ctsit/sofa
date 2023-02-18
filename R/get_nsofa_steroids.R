#' Calculate steroid score
#'
#' @param read_child_medications  the raw child medication csv file obtained from the IDR
#' @param child_encounter a dataframe returned by  \code{\link{get_nsofa_child_encounter}}
#'
#' @return A data frame with the calculated steroid score
#'
#' @export
get_nsofa_steroids <- function(read_child_medications, child_encounter) {
  drug_route <- c("Intravenous", "Oral", "Per NG tube", "Per G Tube", "Per OG Tube")

  steroids <- read_child_medications %>%
    # only choose subjects that are also in child_encounter data
    dplyr::filter(.data$child_mrn_uf %in% child_encounter$child_mrn_uf) %>%
    dplyr::filter(
      stringr::str_detect(
        .data$med_order_desc,
        "DEXAMETHASONE|HYDROCORTISONE|METHYLPREDNISOLONE"
      ) &
        .data$med_order_route %in% drug_route
    ) %>%
    dplyr::filter(!is.na(.data$mar_action)) %>%
    dplyr::mutate(med_order_desc = stringr::word(.data$med_order_desc, 1)) %>%
    sofa::align_nsofa_drug_start_end_times() %>%
    dplyr::mutate(
      number_steroids = rowSums(.[-c(1, 2)]),
      steroids = dplyr::if_else(.data$number_steroids == 0, 0, 1)
    ) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr , .data$steroids) %>%
    dplyr::distinct()

  return(steroids)
}
