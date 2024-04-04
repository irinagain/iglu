#' Calculate eA1C
#'
#' @description
#' The function ea1c produces eA1C values in a tibble object.
#'
#' @usage
#' ea1c(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding eA1C is returned. If a vector of glucose
#' values is passed, then a tibble object with just the eA1C value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for eA1C values is returned. NA glucose values are
#' omitted from the calculation of the eA1C.
#'
#' eA1C score is calculated by \eqn{(46.7+mean(G))/28.7}
#' where G is the vector of Glucose Measurements (mg/dL).
#'
#' @references
#' Nathan (2008) Translating the A1C assay into estimated average glucose
#' values
#' \emph{Hormone and Metabolic Research} \strong{31} .1473-1478,
#' \doi{10.2337/dc08-0545}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' ea1c(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' ea1c(example_data_5_subject)
#'
#'@author
#'Marielle Hicban

ea1c <- function(data){
  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      eA1C = (46.7+mean(gl, na.rm = TRUE) )/28.7
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
