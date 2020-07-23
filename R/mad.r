#' Put what the function does here, ex:
#' Calculate Mean Absolute Deviation (MAD)
#'
#' @description
#' The function mad produces MAD values in a tibble object.
#'
#' @usage
#' mad(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding MAD value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the MAD value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @details
#'
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for MAD values is returned. NA glucose values are
#' omitted from the calculation of the MAD.
#'
#' MAD is calculated by taking the median of the difference of the
#' glucose readings from their median
#' \eqn{median(|gl-median(gl))|}, where gl is the list of Blood Glucose measurements
#' This is calculated for each day and then the mean across all days is taken
#'
#' @references https://github.com/MRCIEU/GLU
#'
#' @examples
#'
#' data(example_data_1_subject)
#' mad(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' mad(example_data_5_subject)
#'

mad = function(data) {

  med = gl = id = NULL
  rm(list = c("med", "gl", "id"))

  data = check_data_columns(data, time_check = TRUE)
  is_vector = attr(data, "is_vector")

  mad_single = function(subject) {
    subject$date = as.Date(subject$time)
    out = subject %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(mad_day = median(abs(gl-median(gl)), na.rm = TRUE))
    return(mean(out$mad_day))
  }

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(MAD = mad_single(data.frame(gl, time)))

  return(out)
}
