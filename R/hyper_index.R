#' Calculate Hyperglycemia Index
#'
#' @description
#' The function hyper_index produces Hyperglycemia Index values in a tibble object.
#'
#' @usage
#' hyper_index(data, ULTR = 140, a = 1.1, c = 30)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @param ULTR Upper Limit of Target Range, default value is 140 mg/dL.
#'
#' @param a Exponent, generally in the range from 1.0 to 2.0, default value is 1.1.
#'
#' @param c Scaling factor, to display Hyperglycemia Index, Hypoglycemia Index, and IGC on approximately the same numerical range as measurements of HBGI, LBGI and GRADE, default value is 30.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the Hyperglycemia Index values is returned. NA glucose values are
#' omitted from the calculation of the Hyperglycemia Index values.
#'
#' Hyperglycemia Index is calculated by \eqn{n/c * \sum [(hyperBG_j-ULTR) ^{a}]}
#' Here n is the total number of Blood Glucose measurements (excluding NA values), \eqn{hyperBG_j}
#' is the jth Blood Glucose measurement above the ULTR cutoff, a is an exponent, and c is a scaling factor.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding Hyperglycemia Index value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the Hyperglycemia Index value is returned.
#' as.numeric() can be wrapped around the latter to ouput just a numeric value.
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
#' hyper_index(example_data_1_subject)
#' hyper_index(example_data_1_subject, ULTR = 160)
#'
#' data(example_data_5_subject)
#' hyper_index(example_data_5_subject)
#' hyper_index(example_data_5_subject, ULTR = 150)
#'


hyper_index <- function(data, ULTR = 140, a = 1.1, c = 30){
  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      hyper_index = sum((gl[gl > ULTR] - ULTR) ^ a, na.rm = TRUE) /
        (length(!is.na(gl)) * c)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
