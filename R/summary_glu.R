#' Calculate summary glucose level
#'
#' @description The function `summary_glu` is a wrapper for the base function
#' `summary()`. Output is a tibble object with subject id and the summary value:
#' Minimum, 1st Quantile, Median, Mean, 3rd Quantile and Max.
#'
#' @usage
#' summary_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for each of summary values is returned. `NA` glucose values are
#' omitted from the  calculation of the summary values.
#'
#' @return If a DataFrame object is passed, then a tibble object with
#' a column for subject id and then a column for each summary value is returned. If a vector of glucose
#' values is passed, then a tibble object without the subject id is returned.
#' `as.numeric()` can be wrapped around the latter to output a numeric vector with
#' values in order of Min, 1st Quantile, Median, Mean, 3rd Quantile and Max.
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

  . = gl = id = NULL
  rm(list = c("gl", "id", "."))
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
