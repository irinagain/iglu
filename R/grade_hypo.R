#' Percentage of GRADE score attributable to hypoglycemia
#'
#' @description
#' The function grade_hypo produces \%GRADE hypoglycemia values in a tibble object.
#'
#' @usage
#' grade_hypo(data, lower = 80)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @param lower Lower bound used for hypoglycemia cutoff, in mg/dL. Default is 80
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding \%GRADE hypoglycemia value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the \%GRADE hypoglycemia value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for \%GRADE hypoglycemia values is returned. NA glucose values are
#' omitted from the calculation of the \%GRADE hypoglycemia values.
#'
#' \%GRADE hypoglycemia is determined by calculating the percentage of
#' GRADE score (see grade function) attributed to hypoglycemic glucose values.
#'
#' @return
#'
#' @export
#'
#' @references
#' Hill et al. (2007): A method for assessing quality of control
#' from glucose profiles
#' \emph{Diabetic Medicine} \strong{24} .753-758,
#' \doi{10.1111/j.1464-5491.2007.02119.x}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' grade_hypo(example_data_1_subject)
#' grade_hypo(example_data_1_subject, lower = 70)
#'
#' data(example_data_5_subject)
#' grade_hypo(example_data_5_subject)
#' grade_hypo(example_data_5_subject, lower = 65)
#'

grade_hypo <- function(data, lower = 80){

  grade = gl = id = NULL
  rm(list = c("grade", "gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::mutate(grade = grade_formula(gl)) %>%
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
