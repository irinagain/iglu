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
#' The function plot_glu contains several plotting types for single and multiple
#' subject data.
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#'
#' @param plottype String corresponding to plot type, currently supported
#' options are 'tsplot' for a 1 subject time series line plot, 'unsorted'
#' for an unsorted multi-subject lasagna plot, and 'rowsorted' for a
#' row-sorted lasagna plot.
#'
#' @param LLTR Lower Limit of Target Range, default value is 80 mg/dL.
#' @param ULTR Upper Limit of Target Range, default value is 140 mg/dL.
#'
#' @param subjects String or list of strings corresponding to subject names
#' in 'id' column of data.
#'
#' @details
#' For the 'tsplot' option, only the first subject will be used, whether the subjects
#' argument is left NULL or a subject list is provided. To choose a particular subject,
#' it is best to only list that subject's id in the subjects argument of plot_glu.
#' The lower and upper values are shown as horizontal lines on the plot to show
#' the target range.
#'
#' For the 'unsorted' and 'rowsorted' options, all subjects are used if the subjects
#' argument is left NULL. If a list of subject IDs is provided, all of those will
#' be used.
#'
#' @return Any output from the plot object
#'
#' @export
#'
#' @examples
#'
#' data(example_data_1_subject)
#' plot_glu(example_data_1_subject)
#' plot_glu(example_data_1_subject, LLTR = 70)
#'
#' data(example_data_5_subject)
#' plot_glu(example_data_5_subject, subjects = 'Subject 2')
#' plot_glu(example_data_5_subject, plottype = 'unsorted')
#' plot_glu(example_data_5_subject, plottype = 'rowsorted')
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
