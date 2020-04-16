#' Calculate Low Blood Glucose Index (LBGI)
#'
#' @description
#' The function lbgi produces LBGI values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' lbgi(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of LBGI.
#'
#' @return A data.frame with the index/measure and identifier column, if
#' the input data is a `data.frame`
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' LBGI is calculated by \eqn{1/n * \sum (10 * fbg_i ^2)},
#' where \eqn{fbg_i = min(0, 1.509 * (log(BG_i)^{1.084} - 5.381)},
#' BG_i is the ith Blood Glucose measurement for a subject, and
#' n is the total number of measurements for that subject.
#'
#' Wrapping as.numeric() around the lbgi call on a dataset with
#' a single subject will return a numeric value corresponding
#' to the LBGI value. This will not work for datasets with multiple subjects.
#'
#' @references
#' Kovatchev et al. (2006) Evaluation of a New Measure of Blood Glucose Variability in,
#' Diabetes
#' \emph{Diabetes care} \strong{29} .2433-2438,
#' \doi{10.2337/dc06-1085}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' lbgi(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' lbgi(example_data_5_subject)
#'

lbgi <- function(data){

  fbg = gl = id = NULL
  rm(list = c("fbg", "gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::mutate(
      fbg = log(gl)^{1.084} - 5.381,
      fbg = pmin(fbg, 0)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      lbgi = 22.77  *
        sum(fbg[ gl <  112.5]^2, na.rm = TRUE) /
        sum(!is.na(gl))
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}

