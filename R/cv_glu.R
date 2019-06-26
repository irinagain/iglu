#' Calculate Coefficient of Variation (CV) of glucose levels
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#'
#' @description The coefficient of variation is computed by dividing
#' the sample standard deviation by the sample mean and multypling by 100%
#'
#' @return
#'
#' @export
#'
#' @examples
#' cv_glu(data)

cv_glu <- function(data){
  gl_by_id = as.double(data$gl)
  out = sd(gl_by_id, na.rm = T) / mean(gl_by_id, na.rm = T) * 100
  return(out)
}
