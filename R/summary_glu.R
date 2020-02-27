#' Calculate summary glucose level
#'
#' @description The function summary_glu is a wrapper for the base function
#' summary(). The output is in a data.frame form by default, with one column
#' and a row corresponding to each subject.
#'
#' @usage
#' summary_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of summary statistics.
#'
#' @details
#' A dataframe structure with 1 row for each subject and either 6 columns
#' corresponding to minimum, 1st quantile, median, mean, 3rd quantile, and
#' maximum value respectively, or 7 columns with the same 6 in order with
#' the number of NA's if any exist in the data.
#'
#' Wrapping as.numeric() around the summary_glu call on a dataset with
#' a single subject will return a numeric value corresponding to the summary.
#' This will not work for datasets with multiple subjects.
#'
#' @return
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#' summary_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' summary_glu(example_data_5_subject)
#'

summary_glu <- function(data){

  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::do({
      x = t(summary(.$gl))
      class(x) = "matrix"
      as.data.frame(x, stringsAsFactors = FALSE)
    })
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
