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
  modd_single = function(data, lag){
    data_ip = CGMS2DayByDay(data)
    gl_by_id_ip = data_ip[[1]]
    out = mean(abs(diff(gl_by_id_ip, lag = 1)), na.rm=T)
    out = data.frame(out)
    names(out) = 'modd'
    return(out)
  }
  modd_multi = function(data,targets){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      data_by_id = data[data$id == subjects[row],]
      out_mat[row, 1] = as.numeric(modd_single(data_by_id))
    }

    out = data.frame(out_mat)
    names(out) = 'modd'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    modd_multi(data)
  } else{
    stop("Data must be in a data.frame structure
         with columns for 'id', 'time', and 'gl'")
  }

}
