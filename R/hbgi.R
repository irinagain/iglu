#' Calculate High Blood Glucose Index (HGBI)
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#'
#' @examples
#' hbgi(data)

hbgi <- function(data){
  gl_by_id = as.double(data$gl)
  fbg = 1.509 * ((log(gl_by_id))^1.084 - 5.381)
  out = mean(10 * pmax(fbg, 0)^2, na.rm = T)
  return(out)
}
