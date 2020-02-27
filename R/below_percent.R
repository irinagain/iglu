#' Calculate percentage below targeted values
#'
#' #' @description
#' The function above_percent produces a dataframe of values equal to
#' the percentage of glucose measurements below target values. The output is in
#' data.frame form by default, where the columns correspond to the target
#' values and the output rows correspond to the subjects. The values will be
#' between 0 (no measurements) and 100 (all measurements).
#'
#' @usage
#' below_percent(data, targets_below = c(50, 80))
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double. NA's will be
#' omitted from the glucose values in calculation of percent.
#'
#' @param targets_below Numeric vector of glucose thresholds. Glucose values from
#' data argument will be compared to each value in the targets_below vector.
#' Default list is (50, 80).
#'
#' @details
#' A dataframe structure with 1 row for each subject and a
#' column for each target value is returned.
#'
#' Wrapping as.numeric() around the below_percent call on a dataset with
#' a single subject will return a numeric vector, where the values
#' correspond to the percent of glucose values below each threshold
#' in the order passed in the targets_below argument. This will not work for
#' datasets with multiple subjects.
#'
#' @return
#'
#' @export
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
#'
#' below_percent(example_data_1_subject)
#' below_percent(example_data_1_subject, targets_below = c(50, 100, 180))
#'
#' as.numeric(below_percent(example_data_1_subject))
#'
#' data(example_data_5_subject)
#'
#' below_percent(example_data_5_subject)
#' below_percent(example_data_5_subject, targets_below = c(80, 180))
#'


below_percent <- function(data, targets_below = c(50,80)){

  x = target_val = id = NULL
  rm(list = c("id", "target_val", "x"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  targets_below = as.double(targets_below)
  out = lapply(
    targets_below,
    function(target_val) {
      data = data %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(x = mean(gl <= target_val, na.rm = TRUE) * 100) %>%
        dplyr::mutate(target_val = paste0("below_", target_val))
      data
    })
  out = dplyr::bind_rows(out) %>%
    tidyr::spread(data = ., key = target_val, value = x)
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}




