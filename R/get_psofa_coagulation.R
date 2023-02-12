#' Calculate coagulation score
#' @description Ingests the raw child labs and calculates the coagulation score based on the platelet count.
#'
#'\itemize{
#'   \item Use the last recorded inferred_specimen_datetime within an hour
#'   \item If there are multiple values for the last recorded inferred_specimen_datetime then use the max coagulation_score
#'   }
#'
#' @param read_child_labs the raw child labs csv file obtained from the IDR
#'
#' @return A data frame with the calculated coagulation score
#'
#' @export
get_psofa_coagulation <- function(read_child_labs) {

  coagulation <- read_child_labs %>%
    dplyr::filter(.data$lab_name == 'platelet count' &
                    stringr::str_detect(.data$lab_result, "\\d")) %>%
    dplyr::mutate(
      lab_result = readr::parse_number(.data$lab_result),
      coagulation_score = dplyr::case_when(
        .data$lab_result < 20 ~ 4,
        .data$lab_result < 50 ~ 3,
        .data$lab_result < 100 ~ 2,
        .data$lab_result < 150 ~ 1,
        TRUE ~ 0
      ),
      q1hr = lubridate::floor_date(.data$inferred_specimen_datetime, "1hour")
    ) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    dplyr::slice_max(.data$inferred_specimen_datetime, with_ties = TRUE) %>%
    dplyr::slice_max(.data$coagulation_score, with_ties = FALSE) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$coagulation_score)

  return(coagulation)
}
