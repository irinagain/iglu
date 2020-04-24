#' Calculate Hyperglycemia Index
#'
#' @description
#' The function hyper_index produces Hyperglycemia index values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' hyper_index(data, ULTR = 140)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of Hyperglycemic Index.
#'
#' @param ULTR Upper Limit of Target Range, default value is 140 mg/dL.
#'
#' @param a Exponent, generally in the range from 1.0 to 2.0, efault value is 1.1.
#'
#' @param c Scaling factor,to display Hyperglycemia Index, Hypoglycemia Index, and IGC on approximately the same numerical range as measurements of HBGI, LBGI and GRADE, default value is 30.
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' Hyperglycemia Index is calculated by \eqn{n/c * \sum [(hyperBG_j-ULTR) ^{a}]}
#' Here n is the total number of Blood Glucose measurements (excluding NA values), \eqn{hyperBG_j}
#' is the jth Blood Glucose measurement above the ULTR cutoff, a is an exponent, and c is a scaling factor.
#'
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
