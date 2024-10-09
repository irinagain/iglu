#' Calculate Hypoglycemia Index
#'
#' @description
#' The function hypo_index produces Hypoglycemia index values in a tibble object.
#'
#' @usage
#' hypo_index(data, LLTR = 80, b = 2, d = 30)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @param LLTR Lower Limit of Target Range, default value is 80 mg/dL.
#'
#' @param b Exponent, generally in the range from 1.0 to 2.0, default value is 2.
#'
#' @param d Scaling factor,to display Hyperglycemia Index, Hypoglycemia Index, and IGC on approximately the same numerical range as measurements of HBGI, LBGI and GRADE, default value is 30.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the Hypoglycemia Index values is returned. NA glucose values are
#' omitted from the calculation of the Hypoglycemia Index values.
#'
#' Hypoglycemia Index is calculated by \eqn{\frac{1}{n \cdot d} \sum \left(ULTR - hyperBG_j \right)^b}.
#' Here n is the total number of Glucose measurements (excluding NA values),
#' and \eqn{hypoBG_j} is the jth Glucose measurement below the LLTR cutoff, b is an exponent, and d is a scaling factor.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding Hypoglycemia Index value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the Hypoglycemia Index value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
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
        (sum(!is.na(gl)) * d)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
