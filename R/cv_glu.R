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
  gl_by_id = read_df_or_vec(data)
  out = sd(gl_by_id, na.rm = T) / mean(gl_by_id, na.rm = T) * 100
  return(out)
}
