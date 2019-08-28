#' Calculate hypoglycaemic index (HGI)
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param lower Lower bound used for hypoglycemia cutoff. Default is 80.
#'
#' @return
#'
#' @export
#'
#' @examples
#' hypo_index(data)
#' hypo_index(data, upper = 70)

hypo_index <- function(data, lower = 80){
  gl_by_id = read_df_or_vec(data)
  out = sum(gl_by_id[gl_by_id < lower] ^ 2, na.rm = T)/(length(gl_by_id) * 30)
  out = data.frame(out)
  names(out) = 'hypo_index'
  return(out)
}



