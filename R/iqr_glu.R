#' Calculate glucose IQR
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
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
  gl_by_id = as.double(data$gl)
  out = IQR(gl_by_id, na.rm = T)
  return(out)
}
