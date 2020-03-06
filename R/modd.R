#' Calculate mean difference between glucose values obtained at the same time
#' of day (MODD)
#'
#' @description
#' The function modd produces MODD values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' modd(data, lag = 1)
#'
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. Missing values will be linearly
#' interpolated when close enough to non-missing values.
#'
#' @param lag Integer indicating which lag (# days) to use. Default is 1.
#'
#' @return
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' MODD is calculated by taking the mean of absolute differences between
#' measurements at the same time 1 day away, or more if lag parameter
#' is set to an integer > 1.
#'
#' Wrapping as.numeric() around the modd call on a dataset with
#' a single subject will return a numeric value corresponding
#' to the MODD value. This will not work for datasets with multiple subjects.
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11 Suppl 1},
#' S55-67. \doi{10.1089/dia.2008.0132}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' modd(example_data_1_subject)
#' modd(example_data_1_subject, lag = 2)
#'
#' data(example_data_5_subject)
#' modd(example_data_5_subject, lag = 2)
#'

modd <- function(data, lag = 1){
  modd_single = function(data, lag){
    data_ip = CGMS2DayByDay(data)
    gl_by_id_ip = data_ip[[1]]
    out = mean(abs(diff(gl_by_id_ip, lag = lag)), na.rm=T)
    out = data.frame(out)
    names(out) = 'modd'
    return(out)
  }

  modd_multi = function(data,lag){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      data_by_id = data[data$id == subjects[row],]
      out_mat[row, 1] = as.numeric(modd_single(data_by_id))
    }

    out = data.frame(out_mat)
    names(out) = 'modd'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    modd_multi(data, lag)
  } else{
    stop("Data must be in a data.frame structure
         with columns for 'id', 'time', and 'gl'")
  }

}
