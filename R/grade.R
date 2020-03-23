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
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11 Suppl 1},
#' S55-67. \doi{10.1089/dia.2008.0132}.
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



