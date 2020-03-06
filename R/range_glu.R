#' Calculate glucose level range
#'
#' @description The function range_glu outputs the distance between minimum
#' and maximum glucose values per subject. The output is in a data.frame form
#' by default, with one column and a row corresponding to each subject.
#'
#' @usage
#' range_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of the range.
#'
#' @details
#' A dataframe structure with 1 row for each subject and 1 column
#' for the range value is returned.
#'
#' Wrapping as.numeric() around the range_glu call on a dataset with
#' a single subject will return a numeric value corresponding to the range.
#' This will not work for datasets with multiple subjects.
#'
#' @return
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
