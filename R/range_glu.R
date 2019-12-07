#' Calculate glucose level range
#'
#' @description The function range_glu outputs the distance between minimum
#' and maximum glucose values per subject. The output is in a data.frame form
#' by default, with one column and a row corresponding to each subject.
#'
#' @usage
#' range_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of the range.
#'
#' @details
#' A dataframe structure with 1 row for each subject and 1 column
#' for the range value is returned.
#'
#' Wrapping as.numeric() around the range_glu call on a dataset with
#' a single subject will return a numeric value corresponding to the range.
#' This will not work for datasets with multiple subjects.
#'
#' @return
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#' range_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' range_glu(example_data_5_subject)
#'

range_glu <- function(data){
  range_glu_single = function(data){
    gl_by_id = na.omit(read_df_or_vec(data))
    glucose_range = range(gl_by_id, na.rm = T)
    out = glucose_range[2] - glucose_range[1]
    out = data.frame(out)
    names(out) = 'range'
    return(out)
  }

  range_glu_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      glucose_range = range(gl_by_id, na.rm = T)
      out_mat[row, 1] = glucose_range[2] - glucose_range[1]
    }

    out = data.frame(out_mat)
    names(out) = 'range'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    range_glu_multi(data)
  } else {
    range_glu_single(data)
  }

}
