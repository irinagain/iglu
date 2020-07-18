#' Calculate eA1C
#'
#' @description
#' The function ea1c produces eA1C values a tibble object.
#'
#' @usage
#' ea1c(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding GMI is returned. If a vector of glucose
#' values is passed, then a tibble object with just the GMI value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for GMI values is returned. NA glucose values are
#' omitted from the calculation of the GMI.
#'
#' eA1C score is calculated by \eqn{(46.7+mean(BG))/28.7}
#' where BG is the list of Blood Glucose Measurements.
#'
#' @references
#' Wojcicki (1995) "J"-index. A new proposition of the assessment
#' of current glucose control in diabetic patients
#' \emph{Hormone and Metabolic Research} \strong{27} .41-42,
#' \doi{10.1055/s-2007-979906}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' ea1c(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' ea1c(example_data_5_subject)
#'

ea1c <- function(data){
  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      ea1c = (46.7+mean(gl, na.rm = TRUE) )/28.7
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
