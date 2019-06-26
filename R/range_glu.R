#' Calculate glucose level range
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#'
#' @description Computes distance between min and max glucose value.
#'
#' @return
#'
#' @export
#'
#' @examples
#' range_glu(data)

range_glu <- function(data){
  gl_by_id = as.double(data$gl)
  glucose_range = range(gl_by_id, na.rm = T)
  out = glucose_range[2] - glucose_range[1]
  return(out)
}
