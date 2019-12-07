#' Percentage of GRADE score below target
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param lower Lower bound used for hypoglycemia cutoff, in mg/dL. Default is 80
#'
#' @return
#'
#' @export
#'
#' @examples
#' grade_hypo(data)
#' grade_hypo(data, lower = 80)

grade_hypo <- function(data, lower = 70){
  grade_eugly_single = function(data, lower){
    gl_by_id = na.omit(read_df_or_vec(data))
    grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
    out = sum(grade_vec[gl_by_id < lower])/sum(grade_vec) * 100
    out = data.frame(out)
    names(out) = 'grade_hypo'
    return(out)
  }

  grade_hypo_multi = function(data, lower){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
      out_mat[row, 1] = sum(grade_vec[gl_by_id < lower])/sum(grade_vec) * 100
    }

    out = data.frame(out_mat)
    names(out) = 'grade_hypo'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    grade_hypo_multi(data, lower)
  } else {
    grade_hypo_single(data, upper)
  }
}
