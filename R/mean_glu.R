#' Calculate mean glucose level
#'
#' @description The function mean_glu is a wrapper for the base function
#' mean(). The output is in a data.frame form by default, with one column
#' and a row corresponding to each subject.
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @usage
#' mean_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of mean.
#'
#' @details
#' A dataframe structure with 1 row for each subject and 1 column for the mean
#' value is returned.
#'
#' Wrapping as.numeric() around the mean_glu call on a dataset with
#' a single subject will return a numeric value corresponding to the mean.
#' This will not work for datasets with multiple subjects.
#'
#' @return
#'
#' @export
#'
#' @examples
#' mean_glu(data)
#'

mean_glu <- function(data){
  mean_glu_single = function(data){
    gl_by_id = na.omit(read_df_or_vec(data))
    out = mean(gl_by_id, na.rm = T)
    out = data.frame(out)
    names(out) = 'mean'
    return(out)
  }

  mean_glu_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = mean(gl_by_id, na.rm = T)
    }

    out = data.frame(out_mat)
    names(out) = 'mean'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    mean_glu_multi(data)
  } else {
    mean_glu_single(data)
  }

}
