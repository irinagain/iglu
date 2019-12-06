#' Calculate median glucose level
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description Simple wrapper for median().
#'
#' @return
#'
#' @export
#'
#' @examples
#' median_glu(data)

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
