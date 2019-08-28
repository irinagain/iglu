#' Calculate Low Blood Glucose Index (LGBI)
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @return
#'
#' @export
#'
#' @examples
#' lbgi(data)

  lbgi <- function(data){
    gl_by_id = read_df_or_vec(data)
    fbg = 1.509 * ((log(gl_by_id))^1.084 - 5.381)
  out = mean(10 * pmin(fbg, 0)^2, na.rm = T)
  out = data.frame(out)
  names(out) = 'lbgi'
  return(out)
}
