#' Calculate J-index
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @return
#'
#' @export
#'
#' @description The J-index is calculated differently for each of the
#' two measurement units. For mg/dL (unit = 0), J-index = 0.001 * (mean + sd)^2
#' For mmol/L (unit = 1), J-index = 0.324 * (mean + sd)^2. This method currently is designed for mg/dl.
#' @examples
#' j_index(data)

j_index <- function(data){
  j_index_single = function(data){
    gl_by_id = read_df_or_vec(data)
    out = .001 * (mean(gl_by_id, na.rm = T) + sd(gl_by_id, na.rm = T))^2
    out = data.frame(out)
    names(out) = 'j_index'
    return(out)
  }

  j_index_mult = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = .001 * (mean(gl_by_id, na.rm = T) +
                                  sd(gl_by_id, na.rm = T))^2
    }

    out = data.frame(out_mat)
    names(out) = 'j_index'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    j_index_multi(data)
  } else {
    j_index_single(data)
  }

}

