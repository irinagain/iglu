#' Calculate hypoglycaemic index (HGI)
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param lower Lower bound used for hypoglycemia cutoff. Default is 80.
#'
#' @return
#'
#' @export
#'
#' @examples
#' hypo_index(data)
#' hypo_index(data, upper = 70)

hypo_index <- function(data, lower = 80){
  hypo_index_single = function(data, lower){
    gl_by_id = read_df_or_vec(data)
    out = sum(gl_by_id[gl_by_id < lower] ^ 2, na.rm = T)/(length(gl_by_id) * 30)
    out = data.frame(out)
    names(out) = 'hypo_index'
    return(out)
  }
  hypo_index_multi = function(data, lower){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = sum(gl_by_id[gl_by_id < lower] ^ 1.1, na.rm = T)/
        (length(gl_by_id) * 30)
    }

    out = data.frame(out_mat)
    names(out) = 'hypo_index'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    hypo_index_multi(data)
  } else {
    hypo_index_single(data)
  }
}
