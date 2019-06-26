#' Calculate standard deviation of glucose levels
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#'
#' @description Simple wrapper for sd().
#' @examples
#' sd_glu(data)

sd_glu <- function(data){
  gl_by_id = as.double(data$gl)
  out = sd(gl_by_id, na.rm = T)
  return(out)
}
