#' Calculate percentage of time CGM was active
#'
#' @description
#' The function active_percent produces a tibble object with values equal to the
#' percentage of time, the cgm was active. For example, if a cgm's (5 min frequency) times were 0, 5, 10, 15 and
#' glucose values were missing at 5, then percentage of time, the cgm was active is 75%.
#' The output columns correspond to the subject id and the precentage of time for which the cgm was active,
#' and the output rows correspond to the subjects.
#' The values will be between 0 (no measurements) and 100 (all measurements).
#'
#' @usage
#' active_percent(data, freqCGM = 5)
#'
#' @param data DataFrame object with column names "id", "time", and "gl"
#' @param freqCGM Numeric value of CGM Frequency(In minutes). Default value is 5.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for active_percent values returned is returned.
#'
#' @return If a data.frame object is passed, then a tibble object with two columns: subject id and
#' corresponding active_percent value is returned
#'
#' @export
#'
#' @references
#' Danne et al. (2017) International Consensus on Use of
#' Continuous Glucose Monitoring
#' \emph{Diabetes Care} \strong{40} .1631-1640,
#' \doi{10.2337/dc17-1600}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#'
#' active_percent(example_data_1_subject)
#' active_percent(example_data_1_subject, freqCGM = 15)
#'
#' data(example_data_5_subject)
#'
#' active_percent(example_data_5_subject)
#' active_percent(example_data_5_subject, freqCGM = 5)
#'


active_percent <- function(data, freqCGM = 5) {
  active_percent = gl = id = NULL
  rm(list = c("gl", "id", "active_percent"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      active_percent = (length(time)/as.integer((as.double(max(time, na.rm = TRUE)) - as.double(min(time, na.rm = TRUE)))/(freqCGM*60)))*100
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
