#' Calculate Hyperglycemia Index
#'
#' @description
#' The function hyper_index produces Hyperglycemia index values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' hyper_index(data, upper = 140)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of Hyperglycemic Index.
#'
#' @param upper Upper bound used for hyperglycemia cutoff. Default is 140.
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' Hyperglycemia Index is calculated by \eqn{n/30 * \sum [(hyperBG_j-upper) ^{1.1}]}
#' Where n is the total number of Blood Glucose measurements and \eqn{hyperBG_j}
#' is the jth Blood Glucose measurement above the hyperglycemia cutoff and upper
#' is the hyperglycemia cutoff.
#'
#' Wrapping as.numeric() around the hyper_index call on a dataset with
#' a single subject will return a numeric value corresponding to the
#' Hyperglycemia Index value. This will not work for datasets with multiple subjects.
#'
#' @return
#'
#' @export
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11 Suppl 1},
#' S55-67. \doi{10.1089/dia.2008.0132}.
#'
#' @examples
#' data(example_data_1_subject)
#' hyper_index(example_data_1_subject)
#' hyper_index(example_data_1_subject, upper = 160)
#'
#' data(example_data_5_subject)
#' hyper_index(example_data_5_subject)
#' hyper_index(example_data_5_subject, upper = 150)
#'


hyper_index <- function(data, upper = 140){
  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      hyper_index = sum((gl[gl > upper] - upper) ^ 1.1, na.rm = TRUE) /
        (length(!is.na(gl)) * 30)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
