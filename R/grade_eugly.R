#' Percentage of GRADE score attributable to target range
#'
#' @description
#' The function grade_eugly produces \%GRADE euglycemia values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' grade_eugly(data, lower = 70, upper = 140)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of \%GRADE euglycemia.
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' \%GRADE euglycemia is calculated by calculating the GRADE score (see grade
#' function) just for values in the target range and dividing by the total
#' GRADE score.
#'
#' Wrapping as.numeric() around the grade_eugly call on a dataset with
#' a single subject will return a numeric value corresponding to the \%GRADE
#' euglycemia value. This will not work for datasets with multiple subjects.
#'
#' @param lower Lower bound used for hypoglycemia cutoff, in mg/dL. Default is 70
#' @param upper Upper bound used for hyperglycemia cutoff, in mg/dL. Default is 140.
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
#'
#' data(example_data_1_subject)
#' grade_eugly(example_data_1_subject)
#' grade_eugly(example_data_1_subject, lower = 80, upper = 180)
#'
#' data(example_data_5_subject)
#' grade_eugly(example_data_5_subject)
#' grade_eugly(example_data_5_subject, lower = 80, upper = 160)
#'
grade_eugly <- function(data, lower = 70, upper = 140){

  grade = gl = id = NULL
  rm(list = c("grade", "gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::mutate(grade = grade_formula(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      grade_eugly = sum(grade[gl >= lower & gl <= upper ], na.rm = TRUE) /
        sum(grade, na.rm = TRUE) * 100
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}

