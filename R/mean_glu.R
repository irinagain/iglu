#' Calculate mean glucose level
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description Simple wrapper for mean().
#'
#' @return
#'
#' @export
#'
#' @examples
#' mean_glu(data)

mean_glu <- function(data){
  gl_by_id = read_df_or_vec(data)
  out = mean(gl_by_id, na.rm = T)
  out = data.frame(out)
  names(out) = 'mean'
  return(out)
}
