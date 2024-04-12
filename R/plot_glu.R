tsplot = function(data, LLTR, ULTR, inter_gap, tz = "", log = F){
  gl = date_by_id = id = gap = time_group = NULL
  rm(list = c("gl", "date_by_id", "id", "gap", "time_group"))
  # Optionally convert data to log scale
  if (!lubridate::is.POSIXct(data$time)){ # Check if already in date format
    data$time = as.character(data$time)
    data$time = as.POSIXct(data$time, format='%Y-%m-%d %H:%M:%S', tz = tz)
    # Check if any NAs from conversion, this happens if wrong time format (e.g. 25:00:00) or wrong time zone which will affect daylight savings time
    if (any(is.na(data$time))){
      warning(paste("During time conversion,", sum(is.na(data$time)), "values were set to NA. Check the correct time zone specification."))
    }
  }

  data <- data %>% dplyr::select(c(id, time, gl))

  data <- data[complete.cases(data), ] %>%
    dplyr::group_by(id) %>%
    dplyr::arrange(data.frame(id, time, gl), time) %>%
    dplyr::ungroup() # ensure ascending time by subject
  gaps <- data %>%
    dplyr::mutate(gap = ifelse(difftime(time, dplyr::lag(time), units = "mins") > inter_gap,
                               TRUE, FALSE), row = 1:length(time)) %>%
    dplyr::slice(1, which(gap))
  gaps <- c(gaps$row, nrow(data) + 1)
  data <- data %>%
    dplyr::mutate(time_group = rep(1:(length(gaps) - 1), diff(gaps))) # group by consecutive times to avoid artifacts

    p = ggplot(data = data, aes(x = time, y = gl, group = time_group)) +
    geom_line(size = 1) +
    scale_x_datetime(name = 'Date') +
    geom_hline(yintercept = LLTR, color = 'red') +
    geom_hline(yintercept = ULTR, color = 'red') +
    facet_wrap(~id, scales = "free_x") +
    geom_point(size = 0.3)

    if(log){
      p = p + scale_y_continuous(name = 'Glucose (mg/dL,  semilogarithmic scale).', trans = 'log10')
    }
    else{
      p = p + scale_y_continuous(name = 'Glucose (mg/dL)')

    }
       p
}


#' Plot time series and lasagna plots of glucose measurements
#'
#' @description
#' The function plot_glu supports several plotting methods for both single and multiple
#' subject data.
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' @inheritParams plot_lasagna
#' @param plottype String corresponding to the desired plot type. Options are
#' 'tsplot' for a time series plot and 'lasagna' for a lasagna plot. See the
#' `lasagnatype` parameter for further options corresponding to the 'lasagna' `plottype`.
#' Default is 'tsplot'.
#'
#' @param LLTR Lower Limit of Target Range, default value is 70 mg/dL.
#' @param ULTR Upper Limit of Target Range, default value is 180 mg/dL.
#' @param log Logical value indicating whether log10 of glucose values should be taken, default value is FALSE.
#' When log = TRUE, the glucose values, LLTR, and ULTR will all be log transformed, and time series plots will
#' be on a semilogarithmic scale.
#'
#' @param subjects String or list of strings corresponding to subject names
#' in 'id' column of data. Default is all subjects.
#'
#' @param inter_gap The maximum allowable gap (in minutes). Gaps larger than
#' this will not be connected in the time series plot. The default value is 45 minutes.
#'
#' @param color_scheme String corresponding to the chosen color scheme when the `plottype` is 'lasagna'. By default, 'blue-red' scheme is used, with the values below `LLTR` colored in shades of blue, and values above `ULTR` colored in shades of red. The alternative 'red-orange' scheme mimics AGP output from \code{\link{agp}} with low values colored in red, in-range values colored in green, and high values colored in yellow and orange.
#'
#' @details
#' For the default option 'tsplot', a time series graph for each subject is
#' produced with hypo- and hyperglycemia cutoffs shown as horizontal red lines.
#' The time series plots for all subjects chosen (all by default) are displayed
#' on a grid.
#'
#' The 'lasagna' plot type works best when the datatype argument is set to average.
#'
#' @return Any output from the plot object
#'
#' @export
#'
#' @examples
#'
#' data(example_data_1_subject)
#' plot_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' plot_glu(example_data_5_subject, subjects = 'Subject 2')
#' plot_glu(example_data_5_subject, plottype = 'tsplot', tz = 'EST', LLTR = 70, ULTR = 150)
#' plot_glu(example_data_5_subject, plottype = 'lasagna', lasagnatype = 'timesorted')
#'


plot_glu <- function(data, plottype = c('tsplot', 'lasagna'), datatype = c("all", "average", "single"), lasagnatype = c('unsorted', 'timesorted'), LLTR = 70, ULTR = 180, subjects = NULL, inter_gap = 45, tz = "",  color_scheme = c("blue-red", "red-orange"), log = F){

  plottype = match.arg(plottype)
  datatype = match.arg(datatype)
  lasagnatype = match.arg(lasagnatype)

  id = NULL
  rm(list = c("id"))

  # Only choose the selected subjects
  if (!is.null(subjects)){
    data = data[data$id %in% subjects, ]
  }

  if (plottype == 'tsplot'){
    tsplot(data, LLTR = LLTR, ULTR = ULTR, tz = tz, inter_gap = inter_gap, log = log)
  }else if (datatype == "single"){
      subject = unique(data$id)
      ns = length(subject)
      if (ns > 1){
        subject = subject[1]
        warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
        data = data[which(data$id == subject)]
      }
      if(log){
        plot_lasagna_1subject(data, lasagnatype = lasagnatype, LLTR = LLTR, ULTR = ULTR, tz = tz, color_scheme = color_scheme, log = T, limits = log(c(50,500)), inter_gap = inter_gap, midpoint = log(105))
      }
      else{
        plot_lasagna_1subject(data, lasagnatype = lasagnatype, LLTR = LLTR, ULTR = ULTR, tz = tz, color_scheme = color_scheme, log = F, inter_gap = inter_gap)
      }
  }
  else{
      if(log){
        plot_lasagna(data, datatype = datatype, lasagnatype = lasagnatype, LLTR = LLTR, ULTR = ULTR, tz = tz, color_scheme = color_scheme, log = T, limits = log(c(50,500)), midpoint = log(105), inter_gap = inter_gap)
    }
      else{
        plot_lasagna(data, datatype = datatype, lasagnatype = lasagnatype, LLTR = LLTR, ULTR = ULTR, tz = tz, color_scheme = color_scheme, log = F, inter_gap = inter_gap)
    }
  }
}
