#' Calculate the Mean Absolute Glucose (MAG)
#'
#' @description
#' The function mag calculates the Mean Absolute Glucose change (MAG) using raw
#' or interpolated data.
#'
#' @usage
#' mag(data, n = NULL, dt0 = NULL, inter_gap = 45, tz = "", max_gap = 45,
#' interpolate = FALSE)
#'
#' @param n Integer giving the desired interval in minutes over which to calculate
#' the change in glucose when interpolate = TRUE. Default is the CGM meter's frequency (dt0)
#' to measure change in every reading.
#'
#' @param max_gap Numeric giving the maximum allowed time gap, in minutes, between
#' consecutive glucose readings to be included in the MAG calculation when using raw data
#' (interpolate = FALSE). Default is 45 minutes. If the time gap between two consecutive
#' readings exceeds this threshold, the change in glucose between those readings will not
#' be included in the MAG calculation.
#'
#' @param interpolate Logical indicating whether MAG should be calculated using
#' raw data or linearly interpolated glucose values. Default is FALSE.
#'
#' @inheritParams roc
#'
#' @return A tibble object with two columns: subject id and MAG value.
#'
#' @export
#'
#' @details
#' A tibble object with a column for subject id and a column for MAG values is
#' returned.
#'
#' If interpolate = TRUE, the data is linearly interpolated and MAG is calculated as
#' \eqn{\frac{|\Delta G|}{\Delta t}} where \eqn{|\Delta G|} is the sum of
#' the absolute change in glucose over n-minute intervals (default n = dt0)
#' and \eqn{\Delta t} is the total elapsed time (in hours).
#'
#' If interpolate = FALSE, MAG is calculated as
#' \eqn{\frac{|\Delta G|}{\Delta t}} where \eqn{|\Delta G|} is the sum of
#' absolute changes between consecutive observed glucose readings with time gaps
#' less than or equal to max_gap, and \eqn{\Delta t} is the total time spanned by
#' those valid time gaps (in hours).
#'
#' This yields the Mean Absolute Glucose change (mg/dL per hour).
#'
#' @author Elizabeth Chun, Neo Kok
#'
#' @references
#' Hermanides et al. (2010) Glucose Variability is Associated with Intensive Care Unit
#' Mortality,
#' \emph{Critical Care Medicine} \strong{38(3)} 838-842,
#' \doi{10.1097/CCM.0b013e3181cc4be9}
#'
#' Kohnert et al. (2013) Evaluation of the Mean Absolute Glucose Change as a Measure
#' of Glycemic Variability Using  Continuous Glucose Monitoring Data,
#' \emph{Diabetes Technol Ther.} \strong{15(6)} 448-454,
#' \doi{10.1089/dia.2012.0303}
#'
#'
#' @examples
#'
#' data(example_data_1_subject)
#' mag(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' mag(example_data_5_subject)
#'

mag <- function(data, n = NULL, dt0 = NULL, inter_gap = 45, tz = "",
                         max_gap = 45, interpolate = FALSE) {

  mag_single <- function(data) {

    if(interpolate){

      data_ip = CGMS2DayByDay(data, dt0 = dt0, inter_gap = inter_gap, tz = tz)
      dt0 <- data_ip[[3]]

      if(is.null(n)) {
        n <- dt0
      } else if (n < dt0) {
        message(paste("Parameter n cannot be less than the data collection frequency: " ,
                      dt0, " , function will be evaluated with n = ", dt0, sep = ""))
        n <- dt0
      } else if (n %% dt0 != 0){
        new_n <- round(n/dt0) * dt0
        message(paste("Parameter n must be a multiple of the data collection frequency: ",
                      dt0, " , function will be evaluated with n = ", new_n, sep = ""))
        n <- new_n
      }

      step_cols <- n / dt0

      flat_gl = as.vector(t(data_ip[[1]]))
      idx <- seq(1, length(flat_gl), by = step_cols)
      idx_gl = flat_gl[idx]
      diffs = na.omit(diff(idx_gl))

      mag = sum(abs(diffs)) /
        (length(diffs) * (n/60))

      return(mag)

    } else{

      gl_diffs <- diff(data$gl)
      time_diffs <- as.numeric(difftime(data$time[-1],
                                        data$time[-length(data$time)], units = "mins"))

      valid <- time_diffs <= max_gap

      if(!any(valid)){
        return(NA_real_)
      }

      numerator <- sum(abs(gl_diffs[valid]))
      denominator <- sum(time_diffs[valid]) / 60

      mag <- numerator / denominator
    }

    return(mag)
  }

  id = mag = NULL
  rm(list = c("id", "mag"))
  data = check_data_columns(data)

  if (!is.integer(n) && !is.null(n)) {
    n <- round(n)
    message("Parameter n must be an integer, input has been rounded to nearest
            integer")
  }

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      MAG = mag_single(data.frame(id, time, gl))
    )

  return(out)
}

