#' Plot time series of glucose colored by rate of change
#'
#' @description
#' The function plot_roc produces a time series plot of glucose values colored
#' by categorized rate of change values
#'
#' @usage
#' plot_roc(data, subjects = NULL, timelag = 15, dt0 = NULL, inter_gap = 45, tz = "")
#'
#' @inheritParams roc
#'
#' @param subjects String or list of strings corresponding to subject names
#' in 'id' column of data. Default is all subjects.
#'
#' @return A time series of glucose values colored by ROC categories per subject
#'
#' @export
#'
#' @details
#' For the default, a time series is produced for each subject in which the glucose values are
#' plotted and colored by ROC categories defined as follows. The breaks for the categories are:
#' c(-Inf, -3, -2, -1, 1, 2, 3, Inf) where the glucose is in mg/dl and the ROC values are in mg/dl/min.
#' A ROC of -5 mg/dl/min will thus be placed in the first category and colored accordingly. The breaks
#' for the categories come from the reference paper below.
#'
#' @author Elizabeth Chun, David Buchanan
#'
#' @references
#' Klonoff, D. C., & Kerr, D. (2017)  A Simplified Approach Using Rate of Change Arrows to
#' Adjust Insulin With Real-Time Continuous Glucose Monitoring.
#' \emph{Journal of Diabetes Science and Technology} \strong{11(6)} 1063-1069,
#' \doi{10.1177/1932296817723260}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' plot_roc(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' plot_roc(example_data_5_subject, subjects = 'Subject 5')
#'

plot_roc <- function(data, subjects = NULL, timelag = 15, dt0 = NULL, inter_gap = 45, tz = ""){
  time_single <- function(data) {
    data_ip = CGMS2DayByDay(data, dt0 = dt0, inter_gap = inter_gap, tz = tz)
    dt0 = data_ip[[3]]
    day_one = lubridate::as_datetime(data_ip[[2]][1])
    ndays = length(data_ip[[2]])
    dti = rep(dt0, ndays * 24 * 60 /dt0)
    time_out =  day_one + lubridate::minutes(cumsum(dti))
    return(time_out)
  }

  gl = gl_ip = time_ip = id = roc = category = NULL
  rm(list = c("gl", "id", "roc", "category", "gl_ip", "time_ip"))
  data = check_data_columns(data)

  if (!is.null(subjects)) {
    data = data[data$id %in% subjects, ]
  }

  data = data %>%
    dplyr::group_by(id) %>%
    dplyr::reframe(
      time_ip = time_single(data.frame(id, time, gl)),
      gl_ip = as.vector(t(CGMS2DayByDay(
        data.frame(id, time, gl), dt0 = dt0, inter_gap = inter_gap, tz = tz)[[1]])),
      roc = roc(data.frame(id, time, gl), timelag, dt0, inter_gap, tz)$roc,
      category = cut(
        roc, breaks = c(-Inf, -3, -2, -1, 1, 2, 3, Inf),
        labels = c("-Inf to -3", "-3 to -2", "-2 to -1",
                   "-1 to 1", "1 to 2", "2 to 3", "3 to Inf"))
    )

  colours = c("-Inf to -3" = "#0025FA", "-3 to -2" = "#197DE3",
              "-2 to -1" = "#B3FFF8", "-1 to 1" = "white",
              "1 to 2" = "#FEC7B6", "2 to 3" = "#FB5454",
              "3 to Inf" = "#9F0909")
  ggplot2::ggplot(data = data[complete.cases(data$gl_ip), ],
                  ggplot2::aes(x = time_ip, y = gl_ip, color = category)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_datetime(name = 'Date') +
    ggplot2::scale_y_continuous(name = 'Blood Glucose') +
    ggplot2::facet_wrap(~id, scales = "free_x") +
    ggplot2::scale_color_manual(values = colours, na.value = "gray",
                                name = "Category (mg/dl/min)")
}
