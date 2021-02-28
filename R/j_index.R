#' Calculate J-index
#'
#' @description
#' The function j_index produces J-Index values a tibble object.
#'
#' @usage
#' j_index(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding J-Index value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the J-Index value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for J-Index values is returned. NA glucose values are
#' omitted from the calculation of the J-Index.
#'
#' J-Index score is calculated by \eqn{.001 * [mean(BG) + sd(BG)]^2}
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
#' j_index(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' j_index(example_data_5_subject)
#'

j_index <- function(data){
  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      J_index = 0.001 * (mean(gl, na.rm = TRUE) + sd(gl, na.rm = TRUE))^2
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}

