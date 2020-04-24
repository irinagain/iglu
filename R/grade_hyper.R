#' Percentage of GRADE score attributable to hyperglycemia
#'
#' @description
#' The function grade_hyper produces \%GRADE hyperglycemia values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' grade_hyper(data, upper = 140)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of \%GRADE hyperglycemia.
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' \%GRADE hyperglycemia is determined by calculating the percentage of
#' GRADE score (see grade function) attributed to hyperglycemic glucose values.
#'
#' Wrapping as.numeric() around the grade_hyper call on a dataset with
#' a single subject will return a numeric value corresponding to the \%GRADE
#' hyperglycemia value. This will not work for datasets with multiple subjects.
#'
#' @param upper Upper bound used for hyperglycemia cutoff, in mg/dL. Default is 140.
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
      grade_hyper = sum(grade[gl > upper], na.rm = TRUE) /
        sum(grade, na.rm = TRUE) * 100
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}


