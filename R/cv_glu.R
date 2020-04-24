#' Calculate Coefficient of Variation (CV) of glucose levels
#'
#' @description
#' The function cv_glu produces CV values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' cv_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of CV.
#'
#' @return
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' CV (Coefficient of Variation) is calculated by \eqn{100 * sd(BG) / mean(BG)}
#' Where BG is the list of all Blood Glucose measurements for a subject.
#'
#' Wrapping as.numeric() around the cv_glu call on a dataset with
#' a single subject will return a numeric value corresponding
#' to the CV value. This will not work for datasets with multiple subjects.
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
