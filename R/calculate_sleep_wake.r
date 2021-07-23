#' Calculate metrics for values inside and/or outside a specified time range.
#'
#' @description
#' This function applies a given function to a subset of data filtered by time of day.
#'
#' @param data DataFrame object with column names "id", "time", and "gl".
#'
#' @param FUN Function to be applied to the filtered data.
#'
#' @param sleep_start Numeric between 0-24 signifying the hour at which the time range should start.
#'
#' @param sleep_end Numeric between 0-24 signifying the hour at which the time range should end.
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
#' If sleep_start is an integer, all times within that hour will be included in the range, but if sleep_end is an integer
#' only times up to that hour will be included in the range.
#' If sleep_start is after sleep_end, the data will be filtered to include all hours after sleep_start and all times before sleep_end.
#'
#'
#' @examples
#'
#' data(example_data_1_subject)
#' calculate_sleep_wake(example_data_1_subject, sd_glu, calculate = "sleep")
#'
#' data(example_data_5_subject)
#' calculate_sleep_wake(example_data_5_subject, cogi, targets = c(80, 150),
#' weights = c(.3,.2,.5), calculate = "wake")
#' calculate_sleep_wake(example_data_5_subject, sd_measures, sleep_start = 2,
#' sleep_end = 8, calculate = "both")
#'


calculate_sleep_wake = function(data, FUN, sleep_start = 0, sleep_end = 6, calculate = c("sleep", "wake", "both"), ...) {
  append_str = function(str_vec, to_append) {
    sapply(str_vec, FUN = function (x) {paste0(x, to_append)})
  }
  FUN = match.fun(FUN)
  calculate = tolower(calculate)
  if (sleep_start > sleep_end) {
    filter_gate = (lubridate::hour(data$time) >= sleep_start | lubridate::hour(data$time) < sleep_end)
  } else {
    filter_gate = (lubridate::hour(data$time) >= sleep_start & lubridate::hour(data$time) < sleep_end)
  }
  if (calculate == "sleep") {
    filtered_data = dplyr::filter(data, filter_gate)
    out = FUN(filtered_data, ...)
  } else if (calculate == "wake") {
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
      colnames(out) = c(append_str(innames[2:length(innames)]," sleep"), append_str(outnames[2:length(outnames)]," wake"))
      return(out)
    }
    out = dplyr::left_join(inside, outside, by = "id")
    colnames(out) = c("id", append_str(innames[2:length(innames)]," sleep"), append_str(outnames[2:length(outnames)]," wake"))
  } else {
    stop("Please enter one of 'sleep', 'wake', 'both' for calculate.")
  }
  return(out)
}

