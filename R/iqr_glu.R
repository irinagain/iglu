#' Calculate glucose IQR
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description Simple wrapper for IQR().
#'
#' @return
#'
#' @export
#'
#' @examples
#' iqr_glu(data)

iqr_glu <- function(data){
  iqr_glu_single = function(data){
    gl_by_id = read_df_or_vec(data)
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
