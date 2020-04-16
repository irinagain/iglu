#' Calculate Hypoglycemia Index
#'
#' @description
#' The function hypo_index produces Hypoglycemia index values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' hypo_index(data, LLTR = 70)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of Hypoglycemic Index.
#'
#' @param LLTR Lower Limit of Target Range, default value is 80 mg/dL.
#'
#' @param b Exponent, generally in the range from 1.0 to 2.0, default value is 2.
#'
#' @param d Scaling factor,to display Hyperglycemia Index, Hypoglycemia Index, and IGC on approximately the same numerical range as measurements of HBGI, LBGI and GRADE, default value is 30.
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' Hypoglycemia Index is calculated by \eqn{n/d * \sum [(LLTR-hypoBG_j)^{b}]}
#' Here n is the total number of Blood Glucose measurements (excluding NA values), and \eqn{hypoBG_j} is the jth Blood Glucose measurement below the LLTR cutoff, b is an exponent, and d is a scaling factor.
#'
#' @return
#'
#' @export
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11} .55-67,
#' \doi{10.1089/dia.2008.0132}.
#'
#' @examples
#' data(example_data_1_subject)
#' hypo_index(example_data_1_subject, LLTR = 60)
#'
#' data(example_data_5_subject)
#' hypo_index(example_data_5_subject)
#' hypo_index(example_data_5_subject, LLTR = 70)
#'

hypo_index <- function(data, LLTR = 80, b = 2, d = 30){
  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      hypo_index = sum((LLTR - gl[gl < LLTR]) ^ b, na.rm = TRUE) /
        (length(!is.na(gl)) * d)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
