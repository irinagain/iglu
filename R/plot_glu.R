#' Plot time series and lasagna plots of glucose measurements
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#'
#' @return
#'
#' @export
#'
#' @examples
#' plot_glu(example_data_1_subject)
#' plot_glu(example_data_5_subject)

plot_glu <- function(data, plottype = 'tsplot', hypo = 80, hyper = 140, subjects = NULL){

  if(plottype == 'tsplot'){
    if(is.null(subjects)){
      subject = unique(data$id)[1]
    }
    else {
      subject = subjects[1]
    }
    tsdata = data[which(data$id == subject), ]
    tsplot(tsdata, hypo, hyper)
  }
  else if(plottype == 'unsorted'){
    if(is.null(subjects)){
      unsorted_lasagna(data)
    }
    else {
      unsorted_lasagna(data[which(data$subjects) %in% subjects, ])
    }
  }
  else if(plottype == 'rowsorted'){
    if(is.null(subjects)){
      rowsorted_lasagna(data)
    }
    else {
      rowsorted_lasagna(data[which(data$subjects) %in% subjects, ])
    }
  }
  else {
    stop('Selected plotting type not supported. See ?plot_glu for available plot types.')
  }

}
