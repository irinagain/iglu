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
  gl_by_id = read_df_or_vec(data)
  out = IQR(gl_by_id, na.rm = T)
  return(out)
  out = data.frame(out)
  names(out) = 'iqr'
}
