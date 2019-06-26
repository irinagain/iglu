#' Calculate mean glucose level
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#'
#' @description Simple wrapper for mean().
#'
#' @return
#'
#' @export
#'
#' @examples
#' mean_glu(data)

mean_glu <- function(data){
  gl_by_id = as.double(data$gl)
  out = mean(gl_by_id, na.rm = T)
  return(out)
}
