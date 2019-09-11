#' Calculate standard deviation of glucose levels
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description Simple wrapper for sd().
#'
#' @return
#'
#' @export
#'
#' @examples
#' sd_glu(data)

sd_glu <- function(data){
  sd_glu_single = function(data){
    gl_by_id = read_df_or_vec(data)
    out = sd(gl_by_id, na.rm = T)
    out = data.frame(out)
    names(out) = 'sd'
    return(out)
  }

  sd_glu_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = sd(gl_by_id, na.rm = T)
    }

    out = data.frame(out_mat)
    names(out) = 'sd'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    sd_glu_multi(data)
  } else {
    sd_glu_single(data)
  }

}
