#' Calculate hepatic score
#'
#' @description Ingests the raw child labs and calculates the hepatic score based on the bilirubin level.
#'
#' \itemize{
#'   \item Use the last recorded inferred_specimen_datetime within an hour
#'   \item If there are multiple values for the last recorded inferred_specimen_datetime then use the max hepatic_score
#'   }
#'
#' @param read_child_labs the raw child labs csv file obtained from the IDR
#'
#' @return A data frame with the calculated hepatic score
#'
#' @export
get_psofa_hepatic <- function(read_child_labs) {
  hepatic <- read_child_labs %>%
    dplyr::filter(.data$lab_name == 'bilirubin total' &
                    stringr::str_detect(.data$lab_result, "\\d")) %>%
    dplyr::mutate(
      lab_result = readr::parse_number(.data$lab_result),
      hepatic_score = dplyr::case_when(
        .data$lab_result < 1.2 ~ 0,
        .data$lab_result <= 1.9 ~ 1,
        .data$lab_result <= 5.9 ~ 2,
        .data$lab_result <= 11.9 ~ 3,
        .data$lab_result >= 12 ~ 4,
        TRUE ~ 0
      ),
      q1hr = lubridate::floor_date(.data$inferred_specimen_datetime, "1hour")
    ) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    dplyr::slice_max(.data$inferred_specimen_datetime, with_ties = TRUE) %>%
    dplyr::slice_max(.data$hepatic_score, with_ties = FALSE) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$hepatic_score)

  return(hepatic)
}
