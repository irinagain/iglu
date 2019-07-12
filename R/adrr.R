#' Calculate average daily risk range (ADRR)
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#'
#' @return
#'
#' @export
#'
#' @examples
#' adrr(data)
#'

#source(iglu:::utils)



adrr <- function(data){
  # temp

  data_ip = CGMS2DayByDay(data)
  gl_by_id_ip = data_ip[[1]]

  fBG = 1.509*(log(gl_by_id_ip)^1.084 - 5.381)
  rBG = 10*fBG^2

  rlbg = matrix(0, nrow = nrow(gl_by_id_ip), ncol = ncol(gl_by_id_ip))
  rhbg = matrix(0, nrow = nrow(gl_by_id_ip), ncol = ncol(gl_by_id_ip))

  rlbg[which(fBG<0)] = rBG[which(fBG<0)]
  rhbg[which(fBG>0)] = rBG[which(fBG>0)]

  out = mean(apply(rlbg,1,max) + apply(rhbg,1,max))
  return(out)
}
