#' Calculate metrics for values inside and/or outside a specified time range.
#'
#' @description
#' This function applies a given function to a subset of data filtered by time of day
#'
#' @param data DataFrame object with column names "id", "time", and "gl".
#'
#' @param FUN Function to be applied to the filtered data.
#'
#' @param after_hour Numeric between 0-24 signifying the hour at which the time range should start.
#'
#' @param before_hour Numeric between 0-24 signifying the hour at which the time range should end.
#'
#' @param calculate String determining whether FUN should be applied to values inside or outside the time range. Both separately is an option
#'
#' @param ... Optional arguments which will be passed to FUN
#'
#' @return An object of the same return type as FUN, with columns corresponding to the values returned by FUN. Separated for values inside or outside the time range, if calculate = both.
#'
#' @export
#'
#' @details
#' An object of the same return type as FUN, with the same column names as FUN will be returned.
#' If calculate = "both", there will be columns for FUN applied to both inside and outside values,
#' with either "in range" or "out of range" append to signify whether the statistic was calculated on values
#' which were inside the time range or outside the range.
#'
#' FUN is found by a call to match.fun and typically is either a function or a character string
#' specifying a function to be searched for from the environment of the call to apply.
#' Arguments in ... cannot have the same name as any of the other arguments, and care may be needed to avoid partial matching to FUN.
#' FUN is applied to the data after the data is filtered based on whether its hour falls within the given range.
#' If after_hour is an integer, all times within that hour will be included in the range, but if before_hour is an integer
#' only times up to that hour will be included in the range.
#'
#'
#' @examples
#'
#' data(example_data_1_subject)
#' calculate_day_night(example_data_1_subject, sd_glu)
#'
#' data(example_data_5_subject)
#' calculate_day_night(example_data_5_subject, cogi, targets = c(80, 150), weights = c(.3,.2,.5))
#' calculate_day_night(example_data_5_subject, sd_measures, after_hour = 2, before_hour = 8, calculate = "both")
#'


#param names need to be redone, very unclear
calculate_day_night = function(data, FUN, after_hour = 0, before_hour = 6, calculate = c("inside", "outside", "both"), ...) {
  append_str = function(str_vec, to_append) {
    sapply(str_vec, FUN = function (x) {paste0(x, to_append)})
  }
  if (after_hour > before_hour) {
    stop("after_hour after before_hour. after_hour should be the hour after which (inclusive) you want times to be included.\nIf you want to calculate values for e.g. 6am-1am, consider using calculate = 'outside'\nand setting after_time = 1am, before_time = 6am")
  }
  FUN = match.fun(FUN)
  calculate = tolower(calculate)
  filter_gate = (lubridate::hour(data$time) >= after_hour & lubridate::hour(data$time) < before_hour)
  if (calculate == "inside") {
    filtered_data = dplyr::filter(data, filter_gate)
    out = FUN(filtered_data, ...)
  } else if (calculate == "outside") {
    filtered_data = dplyr::filter(data, !filter_gate)
    out = FUN(filtered_data, ...)
  } else if (calculate == "both") {
    filtered_data = dplyr::filter(data, filter_gate)
    inside = FUN(filtered_data, ...)
    filtered_data = dplyr::filter(data, !filter_gate)
    outside = FUN(filtered_data, ...)
    innames = colnames(inside)
    outnames = colnames(outside)
    if (is.vector(data)) {
      out = inside
      out$outside = outside
      colnames(out) = c(append_str(innames[2:length(innames)]," in range"), append_str(outnames[2:length(outnames)]," out of range"))
      return(out)
    }
    out = dplyr::left_join(inside, outside, by = "id")
    colnames(out) = c("id", append_str(innames[2:length(innames)]," in range"), append_str(outnames[2:length(outnames)]," out of range"))
  } else {
    stop("Please enter one of 'inside', 'outside', 'both' for calculate.")
  }
  return(out)
}

