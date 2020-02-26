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
#' @param lower Numeric value for hypoglycemia cutoff
#'
#' @param upper Numeric value for hyperglycemia cutoff
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
#' plot_glu(example_data_1_subject, lower = 80)
#'
#' data(example_data_5_subject)
#' plot_glu(example_data_5_subject, subjects = 'Subject 2')
#' plot_glu(example_data_5_subject, plottype = 'unsorted')
#' plot_glu(example_data_5_subject, plottype = 'rowsorted')
#'

plot_glu <- function(data,
                     plottype = c('tsplot', 'unsorted', 'rowsorted'),
                     lower = 70, upper = 140, subjects = NULL){

  plottype = match.arg(plottype)
  if(plottype == 'tsplot'){
    if(is.null(subjects)){
      subject = unique(data$id)[1]
    }
    else {
      subject = subjects[1]
    }
    tsdata = data[which(data$id == subject), ]
    res = tsplot(tsdata, lower, upper)
  }
  else if(plottype == 'unsorted'){
    if(is.null(subjects)){
      res = unsorted_lasagna(data)
    }
    else {
      res = unsorted_lasagna(data[which(data$id) %in% subjects, ])
    }
  }
  else if(plottype == 'rowsorted'){
    if(is.null(subjects)){
      res = rowsorted_lasagna(data)
    }
    else {
      res = rowsorted_lasagna(data[which(data$id) %in% subjects, ])
    }
  }
  else {
    stop('Selected plotting type not supported. See ?plot_glu for available plot types.')
  }
  return(res)
}
