#' Calculate mean glucose level
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description Simple wrapper for mean().
#'
#' @return
#'
#' @export
#'
#' @examples
#' mean_glu(data)

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
