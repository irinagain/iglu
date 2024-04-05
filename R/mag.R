#' Calculate the Mean Absolute Glucose (MAG)
#'
#' @description
#' The function mag calculates the mean absolute glucose or MAG.
#'
#' @usage
#' mag(data, n = 60, dt0 = NULL, inter_gap = 45, tz = "")
#'
#' @param n Integer giving the desired interval in minutes over which to calculate
#' the change in glucose. Default is 60 to have hourly (60 minutes) intervals.
#'
#' @inheritParams roc
#'
#' @return A tibble object with two columns: subject id and MAG value
#'
#' @export
#'
#' @details
#' A tibble object with a column for subject id and a column for MAG values is
#' returned.
#'
#' The glucose values are linearly interpolated over a time grid starting at the
#' beginning of the first day of data and ending on the last day of data. Then, MAG
#' is calculated as \eqn{\frac{|\Delta G|}{\Delta t}} where \eqn{|\Delta G|} is
#' the sum of the absolute change in glucose calculated for each interval as specified
#' by n, default n = 60 for hourly change in glucose. The sum is then divided by
#' \eqn{\Delta t} which is the total time in hours.
#'
#' @author Elizabeth Chun
#'
#' @references
#' Hermanides et al. (2010) Glucose Variability is Associated with Intensive Care Unit
#' Mortaility,
#' \emph{Critical Care Medicine} \strong{38(3)} 838-842,
#' \doi{10.1097/CCM.0b013e3181cc4be9}
#'
#' @examples
#'
#' data(example_data_1_subject)
#' mag(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' mag(example_data_5_subject)
#'

mag <- function (data, n = 60L, dt0 = NULL, inter_gap = 45, tz = "") {

  mag_single <- function (data) {
    data_ip = CGMS2DayByDay(data, dt0 = dt0, inter_gap = inter_gap, tz = tz)
    dt0 <- data_ip[[3]]
    if (n < dt0) {
      message(paste("Parameter n cannot be less than the data collection frequency: " ,
                    dt0, " , function will be evaluated with n = ", dt0, sep = ""))
      n <- dt0
    }

    idx = seq(1, ncol(data_ip[[1]]), by = round(n/data_ip[[3]]))
    idx_gl = as.vector(t(data_ip[[1]][, idx]))
    mag = sum(abs(diff(idx_gl)), na.rm = TRUE)/
      (length(na.omit(idx_gl))*n/60)
    return(mag)
  }

  id = mag = NULL
  rm(list = c("id", "mag"))
  data = check_data_columns(data)

  if (!is.integer(n)) {
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

