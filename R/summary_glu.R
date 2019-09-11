#' Calculate summary statistics of glucose levels
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description Simple wrapper for summary().
#'
#' @return
#'
#' @export
#'
#' @examples
#' summary_glu(data)

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
