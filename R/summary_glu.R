#' Calculate summary glucose level
#'
#' @description The function summary_glu is a wrapper for the base function
#' summary(). The output is in a data.frame form by default, with one column
#' and a row corresponding to each subject.
#'
#' @usage
#' summary_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of summary statistics.
#'
#' @details
#' A dataframe structure with 1 row for each subject and either 6 columns
#' corresponding to minimum, 1st quantile, median, mean, 3rd quantile, and
#' maximum value respectively, or 7 columns with the same 6 in order with
#' the number of NA's if any exist in the data.
#'
#' Wrapping as.numeric() around the summary_glu call on a dataset with
#' a single subject will return a numeric value corresponding to the summary.
#' This will not work for datasets with multiple subjects.
#'
#' @return
#'
#' @export
#'
#' @examples
#' summary_glu(data)
#'

summary_glu <- function(data){
  summary_glu_single = function(data){
    gl_by_id = read_df_or_vec(data)
    out = summary(gl_by_id)
    return(out)
  }

  summary_glu_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 6)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, ] = summary(gl_by_id)
    }

    out = data.frame(out_mat)
    names(out) = names(summary(gl_by_id))
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    summary_glu_multi(data)
  } else {
    summary_glu_single(data)
  }
}
