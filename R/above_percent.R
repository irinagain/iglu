#' Calculate percentage of values above target thresholds
#'
#' @description
#' The function above_percent produces a tibble object with values equal to
#' the percentage of glucose measurements above target values. The output columns
#' correspond to the subject id followed by the target values and the
#' output rows correspond to the subjects. The values will be between 0
#' (no measurements) and 100 (all measurements).
#'
#' @usage
#' above_percent(data, targets_above = c(140, 180, 250))
#'
#' @inheritParams mean_glu
#' @param targets_above Numeric vector of glucose thresholds. Glucose values from
#' data argument will be compared to each value in the targets_above vector.
#' Default list is (140, 180, 250).
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' column for each target value is returned. NA's will be omitted from the glucose
#' values in calculation of percent.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' a column for subject id and then a column for each target value is returned. If a vector of glucose
#' values is passed, then a tibble object without the subject id is returned.
#' as.numeric() can be wrapped around the latter to output a numeric vector.
#'
#' @seealso plot_ranges()
#'
#' @export
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11} .55-67,
#' \doi{10.1089/dia.2008.0132}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#'
#' above_percent(example_data_1_subject)
#' above_percent(example_data_1_subject, targets_above = c(100, 150, 180))
#'
#' data(example_data_5_subject)
#'
#' above_percent(example_data_5_subject)
#' above_percent(example_data_5_subject, targets_above = c(70, 170))
#'

above_percent <- function(data, targets_above = c(140, 180, 250)){

  x = target_val = id = NULL
  rm(list = c("id", "target_val", "x"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  targets_above = as.double(targets_above)
  out = lapply(
    targets_above,
    function(target_val) {
      data = data %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(x = mean(gl > target_val, na.rm = TRUE) * 100) %>%
        dplyr::mutate(target_val = paste0("above_", target_val))
      data
    })
  out = dplyr::bind_rows(out)
  out = tidyr::spread(data = out, key = target_val, value = x)
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}


