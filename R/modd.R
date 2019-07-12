#' Calculate mean difference between glucose values obtained at the same time
#' of day (MODD)
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' @param lag Integer indicating which lag (# days) to use. Default is 1.
#'
#' @return
#'
#' @export
#'
#' @examples
#' modd(data)
#' modd(data, lag = 2)
#

#source(iglu:::utils)

modd <- function(data, lag = 1){
  # temp

  data_ip = CGMS2DayByDay(data)
  gl_by_id_ip = data_ip[[1]]
  out = mean(abs(diff(gl_by_id_ip, lag = 1)), na.rm=T)
  return(out)
}
