#' Calculate mean glucose level
#'
#' @description The function mean_glu is a wrapper for the base function
#' mean(). The output is in a data.frame form by default, with one column
#' and a row corresponding to each subject.
#'
#' @usage
#' mean_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl".
#'
#' @details
#' A dataframe structure with 1 row for each subject and 1 column for the mean
#' value is returned. NA glucose values are be omitted from the  calculation of the mean.
#'
#'
#' @return
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
