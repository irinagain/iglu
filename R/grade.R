# Helper for grade to avoid repetition
grade_formula <- function(x){
  grade = 425 * (log10(log10(x/18)) + 0.16)^2
  grade = ifelse(grade > 50, 50, grade)
  return(grade)
}


#' Calculate mean GRADE score
#'
#' @description
#' The function grade produces GRADE score values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' grade(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of GRADE score.
#'
#'
#' @return
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' GRADE score is calculated by \eqn{1/n * \sum [425 *
#' (log(log(BG_i / 18)) + .16)^2]}
#' Where \eqn{BG_i} is the ith Blood Glucose measurement and n is the total
#' number of measurements.
#'
#' Wrapping as.numeric() around the grade call on a dataset with
#' a single subject will return a numeric value corresponding to the GRADE
#' score. This will not work for datasets with multiple subjects.
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
      grade = mean(grade, na.rm = TRUE)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}



