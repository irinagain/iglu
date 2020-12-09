#' Calculate Mean Amplitude of Glycemic Excursions
#'
#' @description
#' The function mage produces MAGE values in a tibble object.
#'
#' @usage
#' mage(data, sd_multiplier = 1)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @param sd_multiplier A numeric value that can change the sd value used
#' to determine size of glycemic excursions used in the calculation.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding MAGE value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the MAGE value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the MAGE values is returned. NA glucose values are
#' omitted from the calculation of MAGE.
#'
#' MAGE is calculated by taking the mean of absolute differences (between
#' each value and the mean) that are greater than the standard deviation.
#' A multiplier can be added to the standard deviation by the sd_multiplier
#' argument.
#'
#' @references
#' Service, F. J. & Nelson, R. L. (1980) Characteristics of glycemic stability.
#' \emph{Diabetes care} \strong{3} .58-62,
#' \doi{10.2337/diacare.3.1.58}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' mage(example_data_1_subject)
#' mage(example_data_1_subject, sd_multiplier = 2)
#'
#' data(example_data_5_subject)
#' mage(example_data_5_subject, sd_multiplier = .9)
#'

mage <- function(data, sd_multiplier = 1){


  abs_diff_mean = gl = id = NULL
  rm(list = c("gl", "id", "abs_diff_mean"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(abs_diff_mean = abs(gl - mean(gl, na.rm = TRUE))) %>%
    dplyr::summarise(
      mage = mean(
        abs_diff_mean[abs_diff_mean > (sd_multiplier * sd(gl, na.rm = TRUE))],
        na.rm = TRUE)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)

}

