#' Lasagna plot of glucose values for 1 subject aligned across times of day
#'
#' @inheritParams above_percent
#' @param plottype String corresponding to plot type, currently supported
#' options are 'unsorted' for an unsorted single-subject lasagna plot, and 'timesorted' for a
#' lasgna plot with glucose values sorted for each time point across days.
#' @param limits
#' @param midpoint
#' @param dt0
#' @param inter_gap
#' @param tz
#'
#' @return
#' @export
#'
#' @examples
#'
#' plot_lasagna_1subject(example_data_1subject)
#'
plot_lasagna_1subject <- function(data, plottype = c('unsorted', 'timesorted'), limits = c(50, 400), midpoint = 125, dt0 = NULL, inter_gap = 60, tz = ""){

  subject = unique(data$id)
  ns = length(subject)
  if (ns > 1){
    subject = subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data %>% dplyr::filter(id == subject)
  }

  # Get measurements on uniform grid from day to day
  data_ip = CGMS2DayByDay(data, tz = tz, dt0 = dt0, inter_gap = inter_gap)
  gl_by_id_ip = data_ip[[1]]
  dt0 = data_ip$dt0
  ndays = nrow(gl_by_id_ip)
  ntimes = ncol(gl_by_id_ip)
  time_grid_hours = cumsum(rep(dt0, 24 * 60 /dt0)) / 60

  title = ""

  if (plottype == 'timesorted'){
    gl_by_id_ip = apply(gl_by_id_ip, 2, sort, decreasing = T, na.last = T)
    title = ", sorted at each time point."

  }else if (plottype != 'unsorted') {
    warning('Selected plotting type', plottype, 'not supported. Creating unsorted plot by default.')
  }

  # Melt the measurements for lasanga plot
  data_l = data.frame(day = rep(data_ip$actual_dates, each = ntimes), hour = rep(time_grid_hours, ndays), glucose = as.vector(t(gl_by_id_ip)))

  p = data_l%>%
    ggplot(aes(x = hour, y = day, fill = glucose)) + geom_tile() + scale_fill_gradient2(low="blue", high="red", limits = limits, midpoint = midpoint) + ylab("Day") + ggtitle(paste0(subject, title, "")) + xlab("Hour") + xlim(c(0, 24))
  return(p)
}
