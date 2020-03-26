#' Calculate sd glucose level
#'
#' @description The function sd_glu is a wrapper for the base function
#' sd(). The output is in a data.frame form by default, with one column
#' and a row corresponding to each subject.
#'
#' @usage
#' sd_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of sd.
#'
#' @details
#' A dataframe structure with 1 row for each subject and 1 column for the sd
#' value is returned.
#'
#' Wrapping as.numeric() around the sd_glu call on a dataset with
#' a single subject will return a numeric value corresponding to the sd.
#' This will not work for datasets with multiple subjects.
#'
#' @return
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
      sd = sd(gl, na.rm = TRUE)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)

}
