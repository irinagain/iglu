#' Calculate Mean Amplitude of Glycemic Excursions
#'
#' @description
#' The function mage produces MAGE values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' mage(data, sd_multiplier = 1)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of MAGE.
#'
#' @param sd_multiplier A numeric value that can change the sd value used
#' to determine size of glycemic excursions used in the calculation.
#' @return
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' MAGE is calculated by taking the mean of absolute differences (between
#' each value and the mean) that are greater than the standard deviation.
#' A multiplier can be added to the standard deviation by the sd_multiplier
#' argument.
#'
#' Wrapping as.numeric() around the mage call on a dataset with
#' a single subject will return a numeric value corresponding to the MAGE value.
#' This will not work for datasets with multiple subjects.
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11 Suppl 1},
#' S55-67. \doi{10.1089/dia.2008.0132}.
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

