
#' Compute sum of Hyperglycemia index and Hypoglycemia index
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param lower Lower bound used for hypoglycemia cutoff. Default is 80.
#' @param upper Upper bound used for hyperglycemia cutoff. Default is 140.
#'
#' @return
#'
#' @export
#'
#' @examples
#' igc(data)
#' igc(data, lower = 70, upper = 160)

igc <- function(data, lower = 80, upper = 140){
  gl_by_id = read_df_or_vec(data)
  hyper = sum(gl_by_id[gl_by_id > upper] ^ 1.1, na.rm = T)/length(gl_by_id * 30)
  hypo = sum(gl_by_id[gl_by_id < lower] ^ 2, na.rm = T)/length(gl_by_id * 30)
  out = hypo + hyper
  return(out)
}
