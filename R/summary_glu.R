#' Calculate summary statistics of glucose levels
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#'
#' @description Simple wrapper for summary().
#' @examples
#' summary_glu(data)

summary_glu <- function(data){
  gl_by_id = as.double(data$gl)
  out = summary(gl_by_id)
  return(out)
}
