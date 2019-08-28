#' Calculate continuous overall net glycemic action (CONGA)
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#'
#' @return
#'
#' @export
#'
#' @examples
#' conga(data)
#

#source(iglu:::utils)



conga <- function(data){
  # temp

  data_ip = CGMS2DayByDay(data)
  gl_by_id_ip = data_ip[[1]]

  out = sd(diff(gl_by_id_ip), na.rm = T)
  out = data.frame(out)
  names(out) = 'conga'
  return(out)
}
