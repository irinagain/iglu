#' Calculate glucose level quantiles
#'
#' @description
#' The function quantile_glu is a wrapper for the base function quantile().
#' The output is in a data.frame form by default, with a column per quantile
#' and a row corresponding to each subject.
#'
#' @usage
#' quantile_glu(data, quantiles = c(0, 25, 50, 75, 100))
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of percent.
#'
#' @param quantiles List of quantile values between 0 and 100.
#'
#' @details
#' A dataframe structure with 1 row for each subject and 1 column
#' for each quantile is returned.
#'
#' The values are scaled from 0-1 to 0-100 to be consistent in output with
#' above_percent, below_percent, and in_range_percent.
#'
#' Wrapping as.numeric() around the quantile_glu call on a dataset with
#' a single subject will return a numeric vector with values corresponding
#' to the quantiles in the order passed in the quantiles argument.
#' This will not work for datasets with multiple subjects.
#'
#' The command quantile_glu(...) / 100 will scale each element down from 0-100
#' to 0-1.
#'
#' @return
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#'
#' quantile_glu(example_data_1_subject)
#' quantile_glu(example_data_1_subject, quantiles = c(0, 33, 66, 100))
#'
#' data(example_data_5_subject)
#'
#' quantile_glu(example_data_5_subject)
#' quantile_glu(example_data_5_subject, quantiles = c(0, 10, 90, 100))

quantile_glu <- function(data, quantiles = c(0, 25, 50, 75, 100)){

  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::do({
      x = t(quantile(.$gl, na.rm = TRUE, probs = quantiles/100))
      colnames(x) = quantiles
      as.data.frame(x, stringsAsFactors = FALSE)
    })
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
