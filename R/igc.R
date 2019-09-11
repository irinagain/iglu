
#' Compute sum of Hyperglycemia index and Hypoglycemia index
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param lower Lower bound used for hypoglycemia cutoff. Default is 80.
#' @param upper Upper bound used for hyperglycemia cutoff. Default is 140.
#'
#' @return
#'
#' @export
#'
#' @examples
#' igc(data)
#' igc(data, lower = 70, upper = 160)

igc <- function(data, lower = 80, upper = 140){
  igc_single = function(data, lower, upper){
    gl_by_id = read_df_or_vec(data)
    hyper = sum(gl_by_id[gl_by_id > upper] ^ 1.1, na.rm = T)/(length(gl_by_id) * 30)
    hypo = sum(gl_by_id[gl_by_id < lower] ^ 2, na.rm = T)/(length(gl_by_id) * 30)
    out = hypo + hyper
    out = data.frame(out)
    names(out) = 'igc'
    return(out)
  }

  igc_multi = function(data, lower, upper){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      hyper = sum(gl_by_id[gl_by_id > upper] ^ 1.1, na.rm = T)/
                                          (length(gl_by_id) * 30)
      hypo = sum(gl_by_id[gl_by_id < lower] ^ 2, na.rm = T)/
                                          (length(gl_by_id) * 30)

      out_mat[row, 1] = hypo + hyper
    }

    out = data.frame(out_mat)
    names(out) = 'igc'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    igcmulti(data, lower, upper)
  } else {
    igcsingle(data, lower, upper)
  }

}

