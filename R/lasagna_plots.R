#' Lasagna plot of glucose values for 1 subject aligned across times of day
#'
#' @inheritParams conga
#' @param lasagnatype \strong{Default: "unsorted".} String corresponding to plot type, currently supported options are 'unsorted' for an unsorted single-subject lasagna plot, 'timesorted' for a lasagna plot with glucose values sorted within each time point across days, and 'daysorted' for a lasagna plot with glucose values sorted within each day across time points.
#' @param limits The minimal and maximal glucose values for coloring grid which is gradient from blue (minimal) to red (maximal), see \code{\link[ggplot2]{scale_fill_gradient2}})
#' @param midpoint The glucose value serving as midpoint of the diverging gradient scale (see \code{\link[ggplot2]{scale_fill_gradient2}}). The default value is 105 mg/dL. The values above are colored in red, and below in blue in the default color_scheme, which can be adjusted.
#' @param dt0 The time frequency for interpolated aligned grid in minutes, the default will match the CGM meter's frequency (e.g. 5 min for Dexcom).
#' @param inter_gap The maximum allowable gap (in minutes) for interpolation of
#' NA glucose values. The values will not be interpolated between
#' the glucose measurements that are more than inter_gap minutes apart.
#' The default value is 45 min.
#' @param LLTR Lower Limit of Target Range, default value is 70 mg/dL.
#' @param ULTR Upper Limit of Target Range, default value is 180 mg/dL.
#' @param log Logical value indicating whether log of glucose values should be taken, default values is FALSE.
#' When log = TRUE the glucose values, limits, midpoint, LLTR, and ULTR will all be log transformed.
#' @param static_or_gui One of "ggplot" or "plotly". \strong{Default: "plotly".} Returns either a ggplot (static image) or Plotly chart (interactive GUI).
#'
#' @param color_scheme String corresponding to the chosen color scheme. By default, 'blue-red' scheme is used, with the values below `LLTR` colored in shades of blue, and values above `ULTR` colored in shades of red. The alternative 'red-orange' scheme mimics AGP output from \code{\link{agp}} with low values colored in red, in-range values colored in green, and high values colored in yellow and orange.
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
#' plot_lasagna_1subject(example_data_1_subject, color_scheme = 'red-orange')
#' plot_lasagna_1subject(example_data_1_subject, lasagnatype = 'timesorted')
#' plot_lasagna_1subject(example_data_1_subject, lasagnatype = 'daysorted')
#' plot_lasagna_1subject(example_data_1_subject, log = TRUE)
#'
plot_lasagna_1subject <- function(data, lasagnatype = c('unsorted', 'timesorted', 'daysorted'), limits = c(50, 500), midpoint = 105, LLTR = 70, ULTR = 180, dt0 = NULL, inter_gap = 45, tz = "", color_scheme = c("blue-red", "red-orange"), log = F,
                                  static_or_gui = c('ggplot', 'plotly')){

  id = glucose = day = NULL
  rm(list = c("id", "glucose", "day"))
  
  static_or_gui = match.arg(static_or_gui)
  
  # Optionally convert data to log scale
  if (log){
    data$gl = log10(data$gl)
    limits = log10(limits)
    midpoint = log10(midpoint)
    LLTR = log10(LLTR)
    ULTR = log10(ULTR)
  }
  
  # Select the color scheme
  color_scheme = match.arg(color_scheme, c("blue-red", "red-orange"))
  if (color_scheme == "blue-red"){
    # Default blue and red
    colors = c("#3182bd", "#deebf7", "white", "#fee0d2", "#de2d26")
  }else{
    # Alternative red and orange as in commercial software
    colors = c("#8E1B1B", "#F92D00", "#48BA3C", "#F9F000", "#F9B500")
  }
  
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
  
  lasagnatype = match.arg(lasagnatype, c('unsorted', 'timesorted', 'daysorted'))
  if (lasagnatype == 'timesorted'){
    gl_by_id_ip = apply(gl_by_id_ip, 2, sort, decreasing = TRUE, na.last = TRUE)
    title = ", sorted within each time point."
    ytitle = "Day (sorted)"
  } else if (lasagnatype == 'daysorted'){
    gl_by_id_ip = t(apply(gl_by_id_ip, 1, sort, decreasing = TRUE, na.last = TRUE))
    title = ", sorted within each day."
    xtitle = "Hour (sorted)"
  }
  
  # Melt the measurements for lasagna plot
  data_l = data.frame(day = rep(data_ip$actual_dates, each = ntimes), 
                      hour = rep(time_grid_hours, ndays), 
                      glucose = as.vector(t(gl_by_id_ip))) %>%
    mutate(tooltip_text = paste0(
      "Day: ", day,
      "<br>Hour: ", round(hour, 2),
      if (!log) {
        paste0("<br>Glucose (mg/dL): ", round(glucose, 1))
      } else {
        paste0("<br>Log10 Glucose: ", round(glucose, 2))
      }
    ))
  
  # Base ggplot
  p <- ggplot(data_l, aes(
    x    = hour, 
    y    = as.character(day), 
    fill = glucose, 
    text = tooltip_text  # <- Important for Plotly
  )) +
    geom_tile() +
    scale_fill_gradientn(
      colors   = colors, 
      na.value = "grey50", 
      values   = scales::rescale(c(limits[1], LLTR, midpoint, ULTR, limits[2])),
      limits   = limits,
      name     = if (!log) "Glucose (mg/dL)" else "log10(Glucose)"
    ) +
    xlab(xtitle) +
    ylab(ytitle) +
    scale_x_continuous(limits = c(0, 24) + c(0, 0.05), expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    ggtitle(paste0(subject, title)) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "grey50"),
      panel.grid.major = element_line(linewidth = 0, linetype = 'solid', colour = "grey50"),
      panel.grid.minor = element_line(linewidth = 0, linetype = 'solid', colour = "grey50")
    )
  
  # If sorted within time, no meaningful day order
  if (lasagnatype == 'timesorted') {
    p <- p + theme(axis.text.y = element_blank())
  }
  
  # Return either static ggplot or interactive plotly
  static_or_gui = match.arg(static_or_gui, c("plotly", "ggplot"))
  if (static_or_gui == "plotly") {
    # Only show the text in the tooltip
    return(
      plotly::ggplotly(
        p, 
        tooltip = "text"
      )
    )
  }
  
  return(p)
}


#' Lasagna plot of glucose values for multiple subjects
#'
#' @inheritParams plot_lasagna_1subject
#' @param datatype String corresponding to data aggregation used for plotting, currently supported options are 'all' which plots all glucose measurements within the first \code{maxd} days for each subject, and 'average' which plots average 24 hour glucose values across days for each subject
#' @param lasagnatype String corresponding to plot type when using \code{datatype = "average"}, currently supported options are 'unsorted' for an unsorted lasagna plot, 'timesorted' for a lasagna plot with glucose values sorted within each time point across subjects, and '`subjectsorted`' for a lasagna plot with glucose values sorted within each subject across time points.
#' @param maxd For datatype "all", maximal number of days to be plotted from the study. The default value is 14 days (2 weeks).
#' @param LLTR Lower Limit of Target Range, default value is 70 mg/dL.
#' @param ULTR Upper Limit of Target Range, default value is 180 mg/dL.
#' @param log Logical value indicating whether log10 of glucose values should be taken, default value is FALSE.
#' When log = TRUE the glucose values, limits, midpoint, LLTR, and ULTR will all be log transformed.
#' @param static_or_gui One of "ggplot" or "plotly". \strong{Default: "plotly".} Returns either a ggplot (static image) or Plotly chart (interactive GUI).
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
plot_lasagna <- function(data, datatype = c("all", "average"), lasagnatype = c('unsorted', 'timesorted', 'subjectsorted'),
                         maxd = 14, limits = c(50, 500), midpoint = 105, LLTR = 70, ULTR = 180, dt0 = NULL, inter_gap = 45, tz = "",
                         color_scheme = c("blue-red", "red-orange"), log = F, static_or_gui = c('ggplot', 'plotly')){

   lasagnatype = match.arg(lasagnatype, c('unsorted', 'timesorted', 'subjectsorted'))
  datatype = match.arg(datatype, c("all", "average"))
  static_or_gui = match.arg(static_or_gui)
  
  id = glucose = day = NULL
  rm(list = c("id", "glucose", "day"))
  
  # Optionally convert data to log scale
  if (log){
    data$gl = log10(data$gl)
    limits = log10(limits)
    midpoint = log10(midpoint)
    LLTR = log10(LLTR)
    ULTR = log10(ULTR)
  }
  
  subject = unique(data$id)
  ns = length(subject)
  
  # Select the color scheme
  color_scheme = match.arg(color_scheme, c("blue-red", "red-orange"))
  if (color_scheme == "blue-red"){
    # Default blue and red
    colors = c("#3182bd", "#deebf7", "white", "#fee0d2", "#de2d26")
  }else{
    # Alternative red and orange as in commercial software
    colors = c("#8E1B1B", "#F92D00", "#48BA3C", "#F9F000", "#F9B500")
  }
  
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
    
    # Melt the measurements for lasagna plot
    data_l = data.frame(subject = rep(subject, 
                                      each = length(time_grid_hours)), 
                        hour = rep(time_grid_hours, ns), 
                        glucose = as.vector(t(average24))) %>%
      mutate(tooltip_text = paste0(
        "<br>Hour: ", round(hour, 2),
        if (!log) {
          paste0("<br>Glucose (mg/dL): ", round(glucose, 1))
        } else {
          paste0("<br>Log10 Glucose: ", round(glucose, 2))
        }
      ))
    
    
    p = data_l%>%
      ggplot(aes(x = hour, y = subject, fill = glucose, text = tooltip_text)) + 
      geom_tile() + 
      ylab(ytitle) + 
      ggtitle(paste0("Average glucose values for all subjects across days", title, "")) + 
      xlab(xtitle) + 
      scale_x_continuous(limits = c(0, 24) + c(0, 0.05), expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      scale_fill_gradientn(colors = colors, na.value = "grey50", values = scales::rescale(c(limits[1], LLTR, midpoint, ULTR, limits[2])), limits = limits)
      ggtitle(paste0(subject, title)) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "grey50"),
        panel.grid.major = element_line(linewidth = 0, linetype = 'solid', colour = "grey50"),
        panel.grid.minor = element_line(linewidth = 0, linetype = 'solid', colour = "grey50"))
        
    
    if(log){
      p = p + ggplot2::labs(fill = 'log(glucose)')
    }
    
    # Take out subject names if sorted within time since each subject changes
    if (lasagnatype == 'timesorted'){
      p = p + theme(axis.text.y=element_blank())
    }
    
    static_or_gui = match.arg(static_or_gui, c("plotly", "ggplot"))
    if (static_or_gui == "plotly") {
      return(
        plotly::ggplotly(
          p, 
          tooltip = "text"
        )
      )
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
    
    data_l = data.frame(subject = rep(subject, each = nt * max_days), 
                        day = rep(time_grid_days, ns), 
                        glucose = as.vector(t(out))) %>%
      mutate(tooltip_text = paste0(
        "Day: ", round(day, 2),
        if (!log) {
          paste0("<br>Glucose (mg/dL): ", round(glucose, 1))
        } else {
          paste0("<br>Log10 Glucose: ", round(glucose, 2))
        }
      ))
    
    p = data_l%>%
      ggplot(aes(x = day + 1, y = subject, fill = glucose, text = tooltip_text)) + 
      geom_tile() +
      ylab(ytitle) + 
      ggtitle(paste0("All subjects", title)) + 
      xlab(xtitle) + 
      geom_vline(xintercept = c(1:max_days)) + 
      scale_x_continuous(breaks = seq(1, max_days, by = 2), expand = c(0,0)) + 
      scale_y_discrete(expand = c(0,0)) +
      scale_fill_gradientn(colors = colors, na.value = "grey50", values = scales::rescale(c(limits[1], LLTR, midpoint, ULTR, limits[2])), limits = limits) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = "grey50"),
        panel.grid.major = element_line(linewidth = 0, linetype = 'solid', colour = "grey50"),
        panel.grid.minor = element_line(linewidth = 0, linetype = 'solid', colour = "grey50")
      )
    
      
    if(log){
      p = p + ggplot2::labs(fill = 'log(glucose)')
    }
    
    # Take out subject names if sorted within time since each subject changes
    if (lasagnatype == 'timesorted'){
      p = p + theme(axis.text.y=element_blank())
    }
    
    static_or_gui = match.arg(static_or_gui, c("plotly", "ggplot"))
    if (static_or_gui == "plotly") {
      return(
        plotly::ggplotly(
          p, 
          tooltip = "text"
        )
      )
    }
    
    return(p)
  }
}
