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
  adrr_single = function(data,targets){
    data_ip = CGMS2DayByDay(data)
    gl_by_id_ip = data_ip[[1]]

    fBG = 1.509*(log(gl_by_id_ip)^1.084 - 5.381)
    rBG = 10*fBG^2

    rlbg = matrix(0, nrow = nrow(gl_by_id_ip), ncol = ncol(gl_by_id_ip))
    rhbg = matrix(0, nrow = nrow(gl_by_id_ip), ncol = ncol(gl_by_id_ip))

    rlbg[which(fBG<0)] = rBG[which(fBG<0)]
    rhbg[which(fBG>0)] = rBG[which(fBG>0)]

    out = mean(apply(rlbg,1,max) + apply(rhbg,1,max))
    out = data.frame(out)
    names(out) = 'adrr'
    return(out)
  }

  adrr_multi = function(data,targets){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      data_by_id = data[data$id == subjects[row],]
      out_mat[row, 1] = as.numeric(adrr_single(data_by_id))
    }

    out = data.frame(out_mat)
    names(out) = 'adrr'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    adrr_multi(data)
  } else{
    stop("Data must be in a data.frame structure
         with columns for 'id', 'time', and 'gl'")
  }

}
