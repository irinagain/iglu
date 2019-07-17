#' Calculate glucose level quantiles for multiple subjects
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#'
#' @param quantiles List of values between 0 and 1.
#' @param subjects List of subject id's
#'
#' @description Simple wrapper for quantile().
#'
#' @return
#'
#' @export
#'
#' @examples
#' multi_quantile_glu(data)
#' multi_quantile_glu(data, quantiles = c(0, .33, .66, 1))
#' multi_quantile_glu(data, subjects = 'Subject 1')

multi_quantile_glu <- function(data, quantiles = c(0,0.25,0.5,0.75,1), subjects = unique(data$id)){
  out = NULL
  for(subject in subjects){
  gl_by_id = data[data$id == subject,]$gl
  q = quantile(gl_by_id, na.rm = T, probs = quantiles)
  out = rbind(out, q)
  }
  row.names(out) = subjects
  return(out)
}
