#' Calculate sd glucose level
#'
#' @description The function \code{sd_glu} is a wrapper for the base function
#' \code{sd()}. Output is a tibble object with subject id and sd values.
#'
#' @usage
#' sd_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the sd values is returned. \code{NA} glucose values are
#' omitted from the calculation of the sd.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding sd value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the sd value is returned.
#' \code{as.numeric()} can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#' sd_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' sd_glu(example_data_5_subject)
#'

sd_glu <- function(data){

  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      SD = sd(gl, na.rm = TRUE)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)

}
