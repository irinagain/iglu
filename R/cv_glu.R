#' Calculate Coefficient of Variation (CV) of glucose levels
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description The coefficient of variation is computed by dividing
#' the sample standard deviation by the sample mean and multypling by 100%
#'
#' @return
#'
#' @export
#'
#' @examples
#' cv_glu(data)

cv_glu <- function(data){
  cv_glu_single = function(data){
    gl_by_id = na.omit(read_df_or_vec(data))
    out = sd(gl_by_id, na.rm = T) / mean(gl_by_id, na.rm = T) * 100
    out = data.frame(out)
    names(out) = 'cv'
    return(out)
  }

  cv_glu_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = sd(gl_by_id, na.rm = T) / mean(gl_by_id,
                                                       na.rm = T) * 100
    }

    out = data.frame(out_mat)
    names(out) = 'cv'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    cv_glu_multi(data)
  } else {
    cv_glu_single(data)
  }

}
