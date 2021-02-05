#' Percentage of GRADE score attributable to hyperglycemia
#'
#' @description
#' The function grade_hyper produces \%GRADE hyperglycemia values in a tibble object.
#'
#' @usage
#' grade_hyper(data, upper = 140)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding \%GRADE hyperglycemia value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the \%GRADE hyperglycemia value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for \%GRADE hyperglycemia values is returned. NA glucose values are
#' omitted from the calculation of the \%GRADE hyperglycemia values.
#'
#' \%GRADE hyperglycemia is determined by calculating the percentage of
#' GRADE score (see grade function) attributed to hyperglycemic glucose values.
#'
#' @param upper Upper bound used for hyperglycemia cutoff, in mg/dL. Default is 140.
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
#' grade_hyper(example_data_1_subject)
#' grade_hyper(example_data_1_subject, upper = 180)
#'
#' data(example_data_5_subject)
#' grade_hyper(example_data_5_subject)
#' grade_hyper(example_data_5_subject, upper = 160)
#'

grade_hyper <- function(data, upper = 140){

  grade = gl = id = NULL
  rm(list = c("grade", "gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::mutate(grade = grade_formula(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      GRADE_hyper = sum(grade[gl > upper], na.rm = TRUE) /
        sum(grade, na.rm = TRUE) * 100
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}


