#' Calculate Median Absolute Deviation (MAD)
#'
#' @description
#' The function mad produces MAD values in a tibble object.
#'
#' @usage
#' mad_glu(data, constant = 1.4826)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @param constant Numeric object which will be multipled by the MAD value. Defaults to 1.4826.
#' Reasons for this default value can be seen in the details section of the documentation of r's base mad method
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
#' glucose readings from their median and multiplying it by a scaling factor
#' \eqn{1.4826*median(|gl-median(gl)|)}, where gl is the list of Blood Glucose measurements.
#'
#'
#' @author David Buchanan, Marielle Hicban
#'
#' @examples
#'
#' data(example_data_1_subject)
#' mad_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' mad_glu(example_data_5_subject)
#'

mad_glu = function(data, constant = 1.4826) {

  gl = id = NULL
  rm(list = c("gl", "id"))

  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(MAD = mad(gl, constant = constant))

  if (is_vector) {
    out$id = NULL
  }

  return(out)
}
