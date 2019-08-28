#' Calculate median glucose level
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description Simple wrapper for median().
#'
#' @return
#'
#' @export
#'
#' @examples
#' mean_glu(data)

median_glu <- function(data){
  gl_by_id = read_df_or_vec(data)
  out = median(gl_by_id, na.rm = T)
  out = data.frame(out)
  names(out) = 'median'
  return(out)
}
