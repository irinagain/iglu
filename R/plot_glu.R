#' Plot time series of glucose measurements
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
      unsorted(data, hypo, hyper)
    }
    else {
      unsorted(data[which(data$subjects) %in% subjects, ], hypo, hyper)
    }
  }

}
