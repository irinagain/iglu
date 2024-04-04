# Helper for grade to avoid repetition
grade_formula <- function(x){
  grade = 425 * (log10(log10(x/18)) + 0.16)^2
  grade = ifelse(grade > 50, 50, grade)
  return(grade)
}

#' Calculate mean GRADE score
#'
#' @description
#' The function grade produces GRADE score values in a tibble object.
#'
#' @usage
#' grade(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding GRADE value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the GRADE value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for GRADE values is returned. NA glucose values are
#' omitted from the calculation of the GRADE.
#'
#' GRADE score is calculated by \eqn{1/n * \sum [425 *
#' (log(log(G_i / 18)) + .16)^2]}
#' Where \eqn{G_i} is the ith Glucose measurement and n is the total
#' number of measurements.
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
#' grade(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' grade(example_data_5_subject)
#'

grade <- function(data){

  grade = gl = id = NULL
  rm(list = c("grade", "gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::mutate(grade = grade_formula(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      GRADE = mean(grade, na.rm = TRUE)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}



