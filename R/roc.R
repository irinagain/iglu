#' Calculate the Rate of Change at each time point (ROC)
#'
#' @description
#' The function roc produces rate of change values in a tibble object.
#'
#' @usage
#' roc(data, timelag = 15, dt0 = NULL, inter_gap = 45, tz = "")
#'
#' @inheritParams conga
#' @inheritParams CGMS2DayByDay
#'
#' @param timelag Integer indicating the time period (# minutes) over which rate
#' of change is calculated. Default is 15, e.g. rate of change is the change in
#' glucose over the past 15 minutes divided by 15.
#'
#' @return A tibble object with two columns: subject id and rate of change values
#'
#' @export
#'
#' @details
#' A tibble object with a column for subject id and a column for ROC values is
#' returned. A ROC value is returned for each time point for all the subjects. Thus
#' multiple rows are returned for each subject. If the rate of change cannot be
#' calculated, the function will return NA for that point.
#'
#' The glucose values are linearly interpolated over a time grid starting at the
#' beginning of the first day of data and ending on the last day of data. Because
#' of this, there may be many NAs at the beginning and the end of the roc values
#' for each subject. These NAs are a result of interpolated time points that do
#' not have recorded glucose values near them because recording had either not
#' yet begun for the day or had already ended.
#'
#' The ROC is calculated as \eqn{\frac{BG(t_i) - BG(t_{i-1})}{t_i - t_{i-1}}}
#' where BG_i is the Blood Glucose measurement at time t_i and BG_{i-1} is the
#' Blood Glucose measurement at time t_{i-1}. The time difference between the points,
#' t_i - t_{i-1}, is selectable and set at a default of 15 minutes.
#'
#' @author Elizabeth Chun, David Buchanan
#'
#' @references
#' Clarke et al. (2009) Statistical Tools to Analyze Continuous Glucose Monitor Data,
#' Diabetes
#' \emph{Diabetes Technology and Therapeutics} \strong{11} S45-S54,
#' \doi{10.1089/dia.2008.0138}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' roc(example_data_1_subject)
#' roc(example_data_1_subject, timelag = 10)
#'
#' data(example_data_5_subject)
#' roc(example_data_5_subject)
#'

roc <- function (data, timelag = 15, dt0 = NULL, inter_gap = 45, tz = "") {

  roc_single = function (data, timelag = 15) {
    data_ip = CGMS2DayByDay(data, dt0 = dt0, inter_gap = inter_gap, tz = tz)
    gl_ip_vec = as.vector(t(data_ip[[1]]))
    dt0 = data_ip[[3]]

    if (timelag < dt0) {
      message(paste("Parameter timelag cannot be less than the data collection frequency: " ,
                    dt0, " , function will be evaluated with timelag = ", dt0, sep = ""))
      timelag <- dt0
    }

    out = c(rep(NA, timelag/dt0),
            diff(gl_ip_vec, lag = timelag/dt0)/timelag)
    return(out)
  }

  id = roc = NULL
  rm(list = c("id", "roc"))
  data = check_data_columns(data)

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      roc = roc_single(data.frame(id, time, gl), timelag)
    )
  return(out)
}
