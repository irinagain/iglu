#' Plot time series of glucose colored by rate of change
#'
#' @description
#' The function plot_roc produces a time series plot of glucose values colored
#' by categorized rate of change values
#'
#' @usage
#' plot_roc(data, subjects = NULL, timelag = 15, tz = "")
#'
#' @inheritParams conga
#'
#' @param subjects String or list of strings corresponding to subject names
#' in 'id' column of data. Default is all subjects.
#'
#' @param timelag Integer indicating the time period (# minutes) over which rate
#' of change is calculated. Default is 15, e.g. rate of change is the change in
#' glucose over the past 15 minutes divided by 15.
#'
#' @return A time series of glucose values colored by ROC categories per subject
#'
#' @export
#'
#' @details
#' For the default, a time series plot is produced for each subject in which the
#' glucose values are plotted and colored by the following categories for ROC:
#' breaks = c(-Inf, -3, -2, -1, 1, 2, 3, Inf) where the glucose is in mg/dl and
#' the ROC values are in mg/dl/min. A ROC of -5 mg/dl/min will thus be placed in
#' category 1 and colored accordingly.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' plot_roc(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' plot_roc(example_data_5_subject)
#' plot_roc(example_data_5_subject, subjects = 'Subject 5')
#'

plot_roc <- function(data, subjects = NULL, timelag = 15, tz = ""){
  time_single <- function(data) {
    dt0 = CGMS2DayByDay(data)[[3]]
    day_one = lubridate::as_datetime(CGMS2DayByDay(data, tz = tz)[[2]][1])
    ndays = length(CGMS2DayByDay(data, tz = tz)[[2]])
    dti = rep(dt0, ndays * 24 * 60 /dt0)
    time_out =  day_one + lubridate::minutes(cumsum(dti))
    return(time_out)
  }

  gl = id = roc = category = NULL
  rm(list = c("gl", "id", "roc", "category"))
  data = check_data_columns(data)

  if (!is.null(subjects)) {
    data = data[data$id %in% subjects, ]
  }

  data = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      time_ip = time_single(data.frame(id, time, gl)),
      gl_ip = as.vector(t(CGMS2DayByDay(
        data.frame(id, time, gl), tz = tz)[[1]])),
      roc = roc(data.frame(id, time, gl), timelag, tz)$roc,
      category = cut(
        roc, breaks = c(-Inf, -3, -2, -1, 1, 2, 3, Inf),
        labels = c("1", "2", "3","4", "5", "6", "7"))
    )

  colours = c("1" = "purple", "2" = "blue", "3" = "cyan", "4" = "darkolivegreen1",
              "5" = "darkgoldenrod1", "6" = "pink", "7" = "red")
  ggplot2::ggplot(data = data[complete.cases(data$gl_ip), ],
                  ggplot2::aes(x = time_ip, y = gl_ip, color = category)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_datetime(name = 'Date') +
    ggplot2::scale_y_continuous(name = 'Blood Glucose') +
    ggplot2::facet_wrap(~id, scales = "free_x") +
    ggplot2::scale_color_manual(values = colours, na.value = "gray") +
    ggplot2::theme_dark()
}
