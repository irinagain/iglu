#' Calculate median glucose level
#'
#' @description The function median_glu is a wrapper for the base function
#' median(). Output is a tibble object with subject id and median values.
#'
#' @usage
#' median_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the median values is returned. NA glucose values are
#' omitted from the  calculation of the median.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding median value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the median value is returned.
#' as.numeric() can be wrapped around the latter to ouput just a numeric value.
#'
#' @return
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#' median_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' median_glu(example_data_5_subject)
#'

median_glu <- function(data){

  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      median = median(gl, na.rm = TRUE)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
