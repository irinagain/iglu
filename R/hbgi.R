#' Calculate High Blood Glucose Index (HGBI)
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @return
#'
#' @export
#'
#' @examples
#' hbgi(data)
#'


hbgi <- function(data){
  gl_by_id = read_df_or_vec(data)
  fbg = 1.509 * ((log(gl_by_id))^1.084 - 5.381)
  out = mean(10 * pmax(fbg, 0)^2, na.rm = T)
  out = data.frame(out)
  names(out) = 'hbgi'
  return(out)
}
