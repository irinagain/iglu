#' Lasagna plot of glucose values for 1 subject aligned across times of day
#'
#' @inheritParams conga
#' @param lasagnatype String corresponding to plot type, currently supported
#' options are 'unsorted' for an unsorted single-subject lasagna plot, 'timesorted' for a lasagna plot with glucose values sorted within each time point across days, and 'daysorted' for a lasagna plot with glucose values sorted within each day across time points.
#' @param limits The minimal and maximal glucose values for coloring grid which is gradient from blue (minimal) to red (maximal), see \code{\link{scale_fill_gradient2}})
#' @param midpoint The glucose value serving as midpoint (white) of the diverging gradient scale (see \code{\link{scale_fill_gradient2}}). The default value is 105 mg/dL. The values above are colored in red, and below in blue.
#' @param dt0 The time frequency for interpolated aligned grid in minutes, the default will match the CGM meter's frequency (e.g. 5 min for Dexcom).
#' @param inter_gap The maximum allowable gap (in minutes) for interpolation of
#' NA glucose values. The values will not be interpolated between
#' the glucose measurements that are more than inter_gap minutes apart.
#' The default value is 60 min.
#' @param LLTR Lower Limit of Target Range, default value is 70 mg/dL.
#' @param ULTR Upper Limit of Target Range, default value is 180 mg/dL.
#'
#' @return A ggplot object corresponding to lasagna plot
#' @export
#'
#' @references
#' Swihart et al. (2010) Lasagna Plots: A Saucy Alternative to Spaghetti Plots, \emph{Epidemiology} \strong{21}(5), 621-625, \doi{10.1097/EDE.0b013e3181e5b06a}
#'
#' @examples
#'
#' plot_lasagna_1subject(example_data_1_subject)
#' plot_lasagna_1subject(example_data_1_subject, lasagnatype = 'timesorted')
#' plot_lasagna_1subject(example_data_1_subject, lasagnatype = 'daysorted')
#'
plot_lasagna_1subject <- function(data, lasagnatype = c('unsorted', 'timesorted', 'daysorted'), limits = c(50, 500), midpoint = 105, LLTR = 70, ULTR = 180, dt0 = NULL, inter_gap = 60, tz = ""){

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
  ytitle = "Day"
  xtitle = "Hour"

  lasagnatype = match.arg(lasagnatype)
  if (lasagnatype == 'timesorted'){
    gl_by_id_ip = apply(gl_by_id_ip, 2, sort, decreasing = TRUE, na.last = TRUE)
    title = ", sorted within each time point."
    ytitle = "Day (sorted)"
  }else if (lasagnatype == 'daysorted'){
    gl_by_id_ip = t(apply(gl_by_id_ip, 1, sort, decreasing = TRUE, na.last = TRUE))
    title = ", sorted within each day."
    xtitle = "Hour (sorted)"
  }

  # Melt the measurements for lasanga plot
  data_l = data.frame(day = rep(data_ip$actual_dates, each = ntimes), hour = rep(time_grid_hours, ndays), glucose = as.vector(t(gl_by_id_ip)))

  # Make a plot
  p = data_l%>%
    ggplot(aes(x = hour, y = as.character(day), fill = glucose)) + geom_tile()  + ylab(ytitle) + ggtitle(paste0(subject, title, "")) + xlab(xtitle) + xlim(c(0, 24)) + scale_fill_gradientn(colors = c("#3182bd", "#deebf7", "white", "#fee0d2", "#de2d26"), na.value = "grey50", values = scales::rescale(c(limits[1], LLTR, midpoint, ULTR, limits[2])), limits = limits)

  # Take out days if sorted within time since each subject changes
  if (lasagnatype == 'timesorted'){
    p = p + scale_y_discrete(labels = NULL)
  }

  return(p)
}


#' Lasagna plot of glucose values for multiple subjects
#'
#' @inheritParams plot_lasagna_1subject
#' @param datatype String corresponding to data aggregation used for plotting, currently supported options are 'all' which plots all glucose measurements within the first \code{maxd} days for each subject, and 'average' which plots average 24 hour glucose values across days for each subject
#' @param lasagnatype String corresponding to plot type when using\code{datatype = "average"}, currently supported options are 'unsorted' for an unsorted lasagna plot, 'timesorted' for a lasagna plot with glucose values sorted within each time point across subjects, and '`subjectsorted`' for a lasagna plot with glucose values sorted within each subject across time points.
#' @param maxd For datatype "all", maximal number of days to be plotted from the study. The default value is 14 days (2 weeks).
#' @param LLTR Lower Limit of Target Range, default value is 70 mg/dL.
#' @param ULTR Upper Limit of Target Range, default value is 180 mg/dL.
#'
#' @return A ggplot object corresponding to lasagna plot
#' @export
#'
#' @references
#' Swihart et al. (2010) Lasagna Plots: A Saucy Alternative to Spaghetti Plots, \emph{Epidemiology} \strong{21}(5), 621-625, \doi{10.1097/EDE.0b013e3181e5b06a}
#'
#' @examples
#'
#' plot_lasagna(example_data_5_subject, datatype = "average", lasagnatype = 'timesorted', tz = "EST")
#' plot_lasagna(example_data_5_subject, lasagnatype = "subjectsorted", LLTR = 100, tz = "EST")
#'
plot_lasagna <- function(data, datatype = c("all", "average"), lasagnatype = c('unsorted', 'timesorted', 'subjectsorted'), maxd = 14, limits = c(50, 500), midpoint = 105, LLTR = 70, ULTR = 180, dt0 = NULL, inter_gap = 60, tz = ""){

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
    average24 = t(sapply(gdall, colMeans, na.rm = TRUE))
    # Time grid for 24 hour period
    time_grid_hours = cumsum(rep(dt0, 24 * 60 /dt0)) / 60

    # Adjust the title and sort if needed
    title = ""
    ytitle = "Subject"
    xtitle = "Hour"
    if (lasagnatype == 'timesorted'){
      average24 = apply(average24, 2, sort, decreasing = TRUE, na.last = TRUE)
      title = ", sorted within each time point."
      ytitle = "Subject (sorted)"
    }else if (lasagnatype == 'subjectsorted'){
      average24 = t(apply(average24, 1, sort, decreasing = TRUE, na.last = TRUE))
      title = ", sorted within each subject."
      xtitle = "Hour (sorted)"
    }

    # Melt the measurements for lasanga plot
    data_l = data.frame(subject = rep(subject, each = length(time_grid_hours)), hour = rep(time_grid_hours, ns), glucose = as.vector(t(average24)))

    p = data_l%>%
      ggplot(aes(x = hour, y = subject, fill = glucose)) + geom_tile() + ylab(ytitle) + ggtitle(paste0("24 hours averages for all subjects", title, "")) + xlab(xtitle) + xlim(c(0, 24)) + scale_fill_gradientn(colors = c("#3182bd", "#deebf7", "white", "#fee0d2", "#de2d26"), na.value = "grey50", values = scales::rescale(c(limits[1], LLTR, midpoint, ULTR, limits[2])), limits = limits)

    # Take out subject names if sorted within time since each subject changes
    if (lasagnatype == 'timesorted'){
      p = p + scale_y_discrete(labels = NULL)
    }

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
    ytitle = "Subject"
    xtitle = "Day"
    if (lasagnatype == 'timesorted'){
      out = apply(out, 2, sort, decreasing = TRUE, na.last = TRUE)
      title = ", sorted within each time point."
      ytitle = "Subject (sorted)"
    }else if (lasagnatype == 'subjectsorted'){
      out = t(apply(out, 1, sort, decreasing = TRUE, na.last = TRUE))
      title = ", sorted within each subject."
      xtitle = "Day (sorted)"
    }


    data_l = data.frame(subject = rep(subject, each = nt * max_days), day = rep(time_grid_days, ns), glucose = as.vector(t(out)))

    p = data_l%>%
      ggplot(aes(x = day + 1, y = subject, fill = glucose)) + geom_tile() + ylab(ytitle) + ggtitle(paste0("All subjects", title)) + xlab(xtitle) + geom_vline(xintercept = c(1:max_days)) + scale_x_continuous(breaks = seq(1, max_days, by = 2)) + scale_fill_gradientn(colors = c("#3182bd", "#deebf7", "white", "#fee0d2", "#de2d26"), na.value = "grey50", values = scales::rescale(c(limits[1], LLTR, midpoint, ULTR, limits[2])), limits = limits)

    # Take out subject names if sorted within time since each subject changes
    if (lasagnatype == 'timesorted'){
      p = p + scale_y_discrete(labels = NULL)
    }

    return(p)
  }
}
