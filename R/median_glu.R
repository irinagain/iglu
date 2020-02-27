#' Calculate median glucose level
#'
#' @description The function median_glu is a wrapper for the base function
#' median(). The output is in a data.frame form by default, with one column
#' and a row corresponding to each subject.
#'
#' @usage
#' median_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of median.
#'
#' @details
#' A dataframe structure with 1 row for each subject and 1 column for the median
#' value is returned.
#'
#' Wrapping as.numeric() around the median_glu call on a dataset with
#' a single subject will return a numeric value corresponding to the median.
#' This will not work for datasets with multiple subjects.
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
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      median = median(gl, na.rm = TRUE)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
