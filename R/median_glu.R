#' Calculate median glucose level
#'
#' @description The function median_glu is a wrapper for the base function
#' median(). The output is in a data.frame form by default, with one column
#' and a row corresponding to each subject.
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @usage
#' median_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of median.
#'
#' @details
#' A dataframe structure with 1 row for each subject and 1 column for the median
#' value is returned.
#'
#' Wrapping as.numeric() around the median_glu call on a dataset with
#' a single subject will return a numeric value corresponding to the median.
#' This will not work for datasets with multiple subjects.
#'
#' @return
#'
#' @export
#'
#' @examples
#' median_glu(data)
#'

median_glu <- function(data){
  median_glu_single = function(data){
    gl_by_id = na.omit(read_df_or_vec(data))
    out = median(gl_by_id, na.rm = T)
    out = data.frame(out)
    names(out) = 'median'
    return(out)
  }

  median_glu_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = median(gl_by_id, na.rm = T)
    }

    out = data.frame(out_mat)
    names(out) = 'median'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    median_glu_multi(data)
  } else {
    median_glu_single(data)
  }

}
