#' Calculate glucose level quantiles
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#' @param quantiles List of values between 0 and 1.
#'
#' @description Simple wrapper for quantile().
#'
#' @return
#'
#' @export
#'
#' @examples
#' quantile_glu(data)
#' quantile_glu(data, quantiles = c(0, .33, .66, 1))

quantile_glu <- function(data, quantiles = c(0,0.25,0.5,0.75,1)){
  quantile_glu_single = function(data, quantiles){
    gl_by_id = read_df_or_vec(data)
    out = quantile(gl_by_id, na.rm = T, probs = quantiles)
    return(out)
  }
  quantile_glu_multi = function(data, quantiles){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = quantile(gl_by_id, na.rm = T, probs = quantiles)
    }

    out = data.frame(out_mat)
    names(out) = 'quantile'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    quantile_glu_multi(data, quantiles)
  } else {
    quantile_glu_single(data, quantiles)
  }
}
