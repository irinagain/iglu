#' Calculate glucose level quantiles
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#' @param quantiles List of values between 0 and 100.
#'
#' @description Simple wrapper for quantile().
#'
#' @return
#'
#' @export
#'
#' @examples
#' quantile_glu(data)
#' quantile_glu(data, quantiles = c(0, 33, 66, 100))

quantile_glu <- function(data, quantiles = c(0, 25, 50, 75, 100)){
  quantile_glu_single = function(data, quantiles){
    gl_by_id = read_df_or_vec(data)
    out = quantile(gl_by_id, na.rm = T, probs = quantiles/100)
    return(out)
  }
  quantile_glu_multi = function(data, quantiles){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = length(quantiles))
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, ] = quantile(gl_by_id, na.rm = T, probs = quantiles/100)
    }

    out = data.frame(out_mat)
    names(out) = quantiles
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    quantile_glu_multi(data, quantiles)
  } else {
    quantile_glu_single(data, quantiles)
  }
}
