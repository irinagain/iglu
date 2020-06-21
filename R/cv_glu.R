#' Calculate Coefficient of Variation (CV) of glucose levels
#'
#' @description
#' The function cv_glu produces CV values in a tibble object.
#'
#' @usage
#' cv_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding CV value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the CV value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for CV values is returned. NA glucose values are
#' omitted from the calculation of the CV.
#'
#' CV (Coefficient of Variation) is calculated by \eqn{100 * sd(BG) / mean(BG)}
#' Where BG is the list of all Blood Glucose measurements for a subject.
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11} .55-67,
#' \doi{10.1089/dia.2008.0132}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' cv_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' cv_glu(example_data_5_subject)
#'

cv_glu <- function(data){

  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      cv = sd(gl, na.rm = TRUE) / mean(gl, na.rm = TRUE) * 100
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
