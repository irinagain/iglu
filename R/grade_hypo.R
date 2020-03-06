#' Percentage of GRADE score attributable to hypoglycemia
#'
#' @description
#' The function grade_hypo produces \%GRADE hypoglycemia values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' grade_hypo(data, lower = 70)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of \%GRADE hypoglycemia.
#'
#' @param lower Lower bound used for hypoglycemia cutoff, in mg/dL. Default is 70
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' \%GRADE hypoglycemia is calculated by calculating the GRADE score (see grade
#' function) just for values below the hypoglycemia cutoff and dividing by the
#' total GRADE score.
#'
#' Wrapping as.numeric() around the grade_hypo call on a dataset with
#' a single subject will return a numeric value corresponding to the \%GRADE
#' hypoglycemia value. This will not work for datasets with multiple subjects.
#'
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
#' grade_hypo(example_data_1_subject)
#' grade_hypo(example_data_1_subject, lower = 80)
#'
#' data(example_data_5_subject)
#' grade_hypo(example_data_5_subject)
#' grade_hypo(example_data_5_subject, lower = 65)
#'

grade_hypo <- function(data, lower = 70){

  grade = gl = id = NULL
  rm(list = c("grade", "gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::mutate(grade = 425*(log10(log10(gl/18)) + 0.16)^2) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      grade_hypo = sum(grade[gl < lower], na.rm = TRUE) /
        sum(grade, na.rm = TRUE) * 100
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
