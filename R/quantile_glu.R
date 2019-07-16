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
  gl_by_id = read_df_or_vec(data)
  out = quantile(gl_by_id, na.rm = T, probs = quantiles)
  return(out)
}
