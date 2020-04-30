#' Calculate glucose level range
#'
#' @description The function range_glu outputs the distance between minimum
#' and maximum glucose values per subject in a tibble object.
#'
#' @usage
#' range_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the range values is returned. NA glucose values are
#' omitted from the calculation of the range.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding range value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the range value is returned.
#' as.numeric() can be wrapped around the latter to ouput just a numeric value.
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#' range_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' range_glu(example_data_5_subject)
#'

range_glu <- function(data){

  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      range = max(gl, na.rm = TRUE) - min(gl, na.rm = TRUE)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
