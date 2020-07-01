#' Calculate glucose level quantiles
#'
#' @description
#' The function quantile_glu is a wrapper for the base function quantile(). Output
#' is a tibble object with columns for subject id and each of the quantiles.
#'
#' @usage
#' quantile_glu(data, quantiles = c(0, 25, 50, 75, 100))
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @param quantiles List of quantile values between 0 and 100.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for each quantile is returned. NA glucose values are
#' omitted from the calculation of the quantiles.
#'
#' The values are scaled from 0-1 to 0-100 to be consistent in output with
#' above_percent, below_percent, and in_range_percent.
#'
#' The command quantile_glu(...) / 100 will scale each element down from 0-100
#' to 0-1.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' a column for subject id and then a column for each quantile value is returned. If a vector of glucose
#' values is passed, then a tibble object without the subject id is returned.
#' as.numeric() can be wrapped around the latter to output a numeric vector.
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

  . = gl = id = NULL
  rm(list = c("gl", "id", "."))
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
