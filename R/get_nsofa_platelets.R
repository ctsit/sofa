#' Calculate platelets score
#' @description Ingests the raw child labs and calculates the coagulation score based on the platelet count.
#'
#'\itemize{
#'   \item Uses the last recorded inferred_specimen_datetime within an hour
#'   }
#'
#' @param read_child_labs the raw child labs csv file obtained from the IDR
#' @param child_encounter a dataframe returned by  \code{\link{get_nsofa_child_encounter}}
#'
#' @return A data frame with the calculated platelets score
#'
#' @export
#'
get_nsofa_platelets <- function(read_child_labs, child_encounter) {
  platelets <- read_child_labs %>%
    # only choose subjects that are also in child_encounter data
    dplyr::filter(.data$child_mrn_uf %in% child_encounter$child_mrn_uf) %>%
    dplyr::filter(.data$lab_name == 'PLATELET COUNT' &
                    stringr::str_detect(.data$lab_result, "\\d")) %>%
    dplyr::mutate(
      # remove special characters from lab resluts. e.x <3
      lab_result = readr::parse_number(.data$lab_result),
      platelets = dplyr::case_when(
        .data$lab_result < 50 ~ 3,
        .data$lab_result < 100 ~ 2,
        .data$lab_result < 150 ~ 1,
        TRUE ~ 0
      ),
      # round down to the nearest hour
      q1hr = lubridate::floor_date(.data$inferred_specimen_datetime, "1 hour")
    ) %>%
    dplyr::group_by(.data$child_mrn_uf, .data$q1hr) %>%
    # choose last recorded value within the 1 hr timepoint
    dplyr::filter(.data$inferred_specimen_datetime == max(.data$inferred_specimen_datetime)) %>%
    dplyr::select(.data$child_mrn_uf, .data$q1hr, .data$platelets)

  return(platelets)
}
