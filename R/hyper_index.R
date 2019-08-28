#' Calculate hyperglycaemic index (HGI)
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param upper Upper bound used for hyperglycemia cutoff. Default is 140.
#'
#' @return
#'
#' @export
#'
#' @examples
#' hyper_index(data)
#' hyper_index(data, upper = 160)

hyper_index <- function(data, upper = 140){
  gl_by_id = read_df_or_vec(data)
  out = sum(gl_by_id[gl_by_id > upper] ^ 1.1, na.rm = T)/(length(gl_by_id) * 30)
  out = data.frame(out)
  names(out) = 'hyper_index'
  return(out)
}



