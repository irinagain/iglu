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
  gl_by_id = read_df_or_vec(data)
  out = sd(gl_by_id, na.rm = T)
  out = data.frame(out)
  names(out) = 'sd'
  return(out)
}
