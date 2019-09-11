#' Calculate Low Blood Glucose Index (LGBI)
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @return
#'
#' @export
#'
#' @examples
#' lbgi(data)

lbgi <- function(data){
  lbgi_single = function(data){
    gl_by_id = read_df_or_vec(data)
    fbg = 1.509 * ((log(gl_by_id))^1.084 - 5.381)
    out = mean(10 * pmin(fbg, 0)^2, na.rm = T)
    out = data.frame(out)
    names(out) = 'lbgi'
    return(out)
  }

  lbgi_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      fbg = 1.509 * ((log(gl_by_id))^1.084 - 5.381)
      out_mat[row, 1] = mean(10 * pmin(fbg, 0)^2, na.rm = T)
    }
    out = data.frame(out_mat)
    names(out) = 'lbgi'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    lbgi_multi(data)
  } else {
    lbgi_single(data)
  }

}

