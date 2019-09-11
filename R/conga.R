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
  conga_single = function(data){
    data_ip = CGMS2DayByDay(data)
    gl_by_id_ip = data_ip[[1]]

    out = sd(diff(gl_by_id_ip), na.rm = T)
    out = data.frame(out)
    names(out) = 'conga'
    return(out)
  }

  conga_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      data_by_id = data[data$id == subjects[row],]
      out_mat[row, 1] = as.numeric(conga_single(data_by_id))
    }
    out = data.frame(out_mat)
    names(out) = 'conga'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    conga_multi(data)
  } else{
    stop("Data must be in a data.frame structure
         with columns for 'id', 'time', and 'gl'")
  }
}
