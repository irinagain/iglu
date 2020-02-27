#' Calculate J-index
#'
#' @description
#' The function j_index produces J-index values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' j_index(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of J-index score.
#'
#'
#' @return
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' J-index score is calculated by \eqn{.001 * [mean(BG) + sd(BG)]^2}
#' where BG is the list of Blood Glucose Measurements.
#'
#' Wrapping as.numeric() around the j_index call on a dataset with
#' a single subject will return a numeric value corresponding to the J-index
#' score. This will not work for datasets with multiple subjects.
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11 Suppl 1},
#' S55-67. \doi{10.1089/dia.2008.0132}.
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
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      j_index = 0.001 * (mean(gl, na.rm = TRUE) + sd(gl, na.rm = TRUE))^2
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}

