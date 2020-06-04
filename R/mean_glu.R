#' Calculate mean glucose level
#'
#' @description The function mean_glu is a wrapper for the base function
#' mean(). Output is a tibble object with subject id and mean values.
#'
#' @usage
#' mean_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the mean values is returned. NA glucose values are
#' omitted from the  calculation of the mean.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding mean value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the mean value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#' mean_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' mean_glu(example_data_5_subject)
#'

mean_glu <- function(data){

  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      mean = mean(gl, na.rm = TRUE)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)

}
