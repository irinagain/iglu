#' Calculate glucose level range
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description Computes distance between min and max glucose value.
#'
#' @return
#'
#' @export
#'
#' @examples
#' range_glu(data)

range_glu <- function(data){
  gl_by_id = read_df_or_vec(data)
  glucose_range = range(gl_by_id, na.rm = T)
  out = glucose_range[2] - glucose_range[1]
  out = data.frame(out)
  names(out) = 'range'
  return(out)
}
