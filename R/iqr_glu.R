#' Calculate glucose level iqr
#'
#' @description The function iqr_glu outputs the distance between the 25th
#' percentile and the 25th percentile of the glucose values for each subject.
#' The output is in a data.frame form by default, with one column and a
#' row corresponding to each subject.
#'
#' @usage
#' iqr_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of the iqr.
#'
#' @details
#' A dataframe structure with 1 row for each subject and 1 column
#' for the iqr value is returned.
#'
#' Wrapping as.numeric() around the iqr_glu call on a dataset with
#' a single subject will return a numeric value corresponding to the iqr.
#' This will not work for datasets with multiple subjects.
#'
#' @return
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#' iqr_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' iqr_glu(example_data_5_subject)
#'

iqr_glu <- function(data){
  iqr_glu_single = function(data){
    gl_by_id = na.omit(read_df_or_vec(data))
    out = IQR(gl_by_id, na.rm = T)
    out = data.frame(out)
    names(out) = 'iqr'
    return(out)
  }

  iqr_glu_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = IQR(gl_by_id, na.rm = T)
    }

    out = data.frame(out_mat)
    names(out) = 'iqr'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    iqr_glu_multi(data)
  } else {
    iqr_glu_single(data)
  }

}
