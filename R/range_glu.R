#' Calculate glucose level range
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description Computes distance between min and max glucose value.
#'
#' @return
#'
#' @export
#'
#' @examples
#' range_glu(data)

range_glu <- function(data){
  range_glu_single = function(data){
    gl_by_id = read_df_or_vec(data)
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
