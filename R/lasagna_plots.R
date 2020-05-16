#' Lasagna plot of glucose values for 1 subject aligned across times of day
#'
#' @inheritParams conga
#' @param lasagnatype String corresponding to plot type, currently supported
#' options are 'unsorted' for an unsorted single-subject lasagna plot, and 'timesorted' for a
#' lasgna plot with glucose values sorted for each time point across days.
#' @param limits The minimal and maximal glucose values for coloring grid which is gradient from blue (minimal) to red (maximal), see \code{\link[ggplot2]{scale_fill_gradient2}})
#' @param midpoint The glucose value serving as midpoint (white) of the diverging gradient scale (see \code{\link[ggplot2]{scale_fill_gradient2}}). The default value is 125 mg/dL. The values above are colored in red, and below in blue.
#' @param dt0 The time frequency for interpolated aligned grid in minutes, the default will match the CGM meter's frequency (e.g. 5 min for Dexcom).
#' @param inter_gap The maximum allowable gap (in minutes) for interpolation of NA glucose values. The values will not be interpolated between the glucose measurements thare are more than inter_gap minutes apart. The default value is 60 min.
#' @param LLTR Lower Limit of Target Range, default value is 80 mg/dL.
#' @param ULTR Upper Limit of Target Range, default value is 140 mg/dL.
#'
#' @return A ggplot object corresponding to lasagna plot
#' @export
#'
#' @examples
#'
#' plot_lasagna_1subject(example_data_1_subject)
#'
plot_lasagna_1subject <- function(data, lasagnatype = c('unsorted', 'timesorted'), limits = c(50, 500), midpoint = 105, LLTR = 80, ULTR = 140, dt0 = NULL, inter_gap = 60, tz = ""){

  id = glucose = day = NULL
  rm(list = c("id", "glucose", "day"))

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

  lasagnatype = match.arg(lasagnatype)
  if (lasagnatype == 'timesorted'){
    gl_by_id_ip = apply(gl_by_id_ip, 2, sort, decreasing = T, na.last = T)
    title = ", sorted at each time point."
  }

  # Melt the measurements for lasanga plot
  data_l = data.frame(day = rep(data_ip$actual_dates, each = ntimes), hour = rep(time_grid_hours, ndays), glucose = as.vector(t(gl_by_id_ip)))

  p = data_l%>%
    ggplot(aes(x = hour, y = day, fill = glucose)) + geom_tile()  + ylab("Day") + ggtitle(paste0(subject, title, "")) + xlab("Hour") + xlim(c(0, 24)) + scale_fill_gradientn(colors = c("#3182bd", "#deebf7", "white", "#fee0d2", "#de2d26"), na.value = "grey50", values = scales::rescale(c(limits[1], LLTR, midpoint, ULTR, limits[2])), limits = limits)

  return(p)
}


#' Lasagna plot of glucose values for multiple subjects
#'
#' @inheritParams plot_lasagna_1subject
#' @param datatype String corresponding to data aggregation used for plotting, currently supported
#' options are 'all' which plots all glucose measurements within the first \code{maxd} days for each subject, and 'average' which plots average 24 hour glucose values across days for each subject
#' @param lasagnatype String corresponding to plot type when using\code{datatype = "average"}, currently supported options are 'unsorted' for an unsorted lasagna plot, and 'timesorted' for a
#' lasagna plot with glucose values sorted for each time point across subjects
#' @param maxd For datatype "all", maximal number of days to be plotted from the study. The default value is 14 days (2 weeks).
#' @param LLTR Lower Limit of Target Range, default value is 80 mg/dL.
#' @param ULTR Upper Limit of Target Range, default value is 140 mg/dL.
#'
#' @return A ggplot object corresponding to lasagna plot
#' @export
#'
#' @examples
#'
#' plot_lasagna(example_data_5_subject, tz = "EST")
#' plot_lasagna(example_data_5_subject, maxd = 18, tz = "EST")
#' plot_lasagna(example_data_5_subject, datatype = "average", tz = "EST")
#' plot_lasagna(example_data_5_subject, datatype = "average", lasagnatype = 'timesorted', tz = "EST")
#'
plot_lasagna <- function(data, datatype = c("all", "average"), lasagnatype = c('unsorted', 'timesorted'), maxd = 14, limits = c(50, 500), midpoint = 105, LLTR = 80, ULTR = 140, dt0 = NULL, inter_gap = 60, tz = ""){

  lasagnatype = match.arg(lasagnatype)
  datatype = match.arg(datatype)

  id = glucose = day = NULL
  rm(list = c("id", "glucose", "day"))

  subject = unique(data$id)
  ns = length(subject)

  # Calculate uniform grid for all subjects
  gdall = list()
  for (i in 1:ns){
    if (i != 1){
      dt0 = out$dt0
    }
    out = data %>%
      dplyr::filter(id == subject[i]) %>%
      CGMS2DayByDay(tz = tz, dt0 = dt0, inter_gap = inter_gap)
    gdall[[i]] <- out$gd2d
  }
  dt0 = out$dt0

  if (datatype == "average"){
    # Combine the list of averages into the matrix form
    average24 = t(sapply(gdall, colMeans, na.rm = T))
    # Time grid for 24 hour period
    time_grid_hours = cumsum(rep(dt0, 24 * 60 /dt0)) / 60

    # Adjust the title and sort if needed
    title = ""
    if (lasagnatype == 'timesorted'){
      average24 = apply(average24, 2, sort, decreasing = T, na.last = T)
      title = ", sorted at each time point."
    }

    # Melt the measurements for lasanga plot
    data_l = data.frame(subject = rep(subject, each = length(time_grid_hours)), hour = rep(time_grid_hours, ns), glucose = as.vector(t(average24)))

    p = data_l%>%
      ggplot(aes(x = hour, y = subject, fill = glucose)) + geom_tile() + ylab("Subject") + ggtitle(paste0("24 hours averages for all subjects", title, "")) + xlab("Hour") + xlim(c(0, 24)) + scale_fill_gradientn(colors = c("#3182bd", "#deebf7", "white", "#fee0d2", "#de2d26"), na.value = "grey50", values = scales::rescale(c(limits[1], LLTR, midpoint, ULTR, limits[2])), limits = limits)
    return(p)
  }else{
    max_days = max(sapply(gdall, function(x) nrow(x)))
    max_days = min(max_days, maxd)
    nt = 24 * 60 /dt0
    time_grid_days = rep(cumsum(rep(dt0, nt)) / (24 * 60), max_days) + rep(c(0:(max_days - 1)), each = nt)
    stretch_select <- function(x){
      nd = nrow(x)
      if (nd < max_days){
        tmp = matrix(NA, max_days, nt)
        tmp[1:nd, ] = x
        as.vector(t(tmp))
      }else{
        as.vector(t(x[1:max_days, ]))
      }
    }

    out = t(sapply(gdall, stretch_select))

    # Adjust the title and sort if needed
    title = ""
    if (lasagnatype == 'timesorted'){
      out = apply(out, 2, sort, decreasing = T, na.last = T)
      title = ", sorted at each time point."
    }


    data_l = data.frame(subject = rep(subject, each = nt * max_days), day = rep(time_grid_days, ns), glucose = as.vector(t(out)))

    p = data_l%>%
      ggplot(aes(x = day + 1, y = subject, fill = glucose)) + geom_tile() + ylab("Subject") + ggtitle(paste0("All subjects", title)) + xlab("Day") + geom_vline(xintercept = c(1:max_days)) + scale_x_continuous(breaks = seq(1, max_days, by = 2)) + scale_fill_gradientn(colors = c("#3182bd", "#deebf7", "white", "#fee0d2", "#de2d26"), na.value = "grey50", values = scales::rescale(c(limits[1], LLTR, midpoint, ULTR, limits[2])), limits = limits)

    return(p)
  }
}
