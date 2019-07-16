#' Calculate summary statistics of glucose levels
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @description Simple wrapper for summary().
#'
#' @return
#'
#' @export
#'
#' @examples
#' summary_glu(data)

summary_glu <- function(data){
  gl_by_id = read_df_or_vec(data)
  out = summary(gl_by_id)
  return(out)
}
