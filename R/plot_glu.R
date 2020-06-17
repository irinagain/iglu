tsplot = function(data, LLTR, ULTR, tz = ""){
  gl = date_by_id = id = NULL
  rm(list = c("gl", "date_by_id", "id"))
  if (!lubridate::is.POSIXct(data$time)){ # Check if already in date format
    data$time = as.character(data$time)
    data$time = as.POSIXct(data$time, format='%Y-%m-%d %H:%M:%S', tz = tz)
    # Check if any NAs from conversion, this happens if wrong time format (e.g. 25:00:00) or wrong time zone which will affect daylight savings time
    if (any(is.na(data$time))){
      warning(paste("During time conversion,", sum(is.na(data$time)), "values were set to NA. Check the correct time zone specification."))
    }
  }
  ggplot2::ggplot(data = data, ggplot2::aes(x = time, y = gl, group = id)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(name = 'Date') +
    ggplot2::scale_y_continuous(name = 'Blood Glucose') +
    ggplot2::geom_hline(yintercept = LLTR, color = 'red') +
    ggplot2::geom_hline(yintercept = ULTR, color = 'red') + ggplot2::facet_wrap(~id, scales = "free_x")
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
#' @param LLTR Lower Limit of Target Range, default value is 80 mg/dL.
#' @param ULTR Upper Limit of Target Range, default value is 140 mg/dL.
#'
#' @param subjects String or list of strings corresponding to subject names
#' in 'id' column of data. Default is all subjects.
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
#' plot_glu(example_data_5_subject, plottype = 'tsplot', tz = 'EST', LLTR = 70, ULTR= 150)
#' plot_glu(example_data_5_subject, plottype = 'lasagna', lasagnatype = 'timesorted')
#'

plot_glu <- function(data, plottype = c('tsplot', 'lasagna'), datatype = c("all", "average", "single"), lasagnatype = c('unsorted', 'timesorted'), LLTR = 80, ULTR = 140, subjects = NULL, tz = ""){

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
    tsplot(data, LLTR = LLTR, ULTR = ULTR, tz = tz)
  }else if (datatype == "single"){
      subject = unique(data$id)
      ns = length(subject)
      if (ns > 1){
        subject = subject[1]
        warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
        data = data[which(data$id == subject)]
      }
    plot_lasagna_1subject(data, lasagnatype = lasagnatype, LLTR = LLTR, ULTR = ULTR, tz = tz)
  }else{
    plot_lasagna(data, datatype = datatype, lasagnatype = lasagnatype, LLTR = LLTR, ULTR = ULTR, tz = tz)
  }
}
