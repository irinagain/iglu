#' Percentage of GRADE score above target
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param upper Upper bound used for hyperglycemia cutoff, in mg/dL. Default is 140.
#'
#' @return
#'
#' @export
#'
#' @examples
#' grade_hyper(example_data_5_subject)
#' grade_hyper(example_data_5_subject, upper = 160)
#'

grade_hyper <- function(data, upper = 140){
  grade_hyper_single = function(data, upper){
    gl_by_id = na.omit(read_df_or_vec(data))
    grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
    out = sum(grade_vec[gl_by_id > upper])/sum(grade_vec) * 100
    out = data.frame(out)
    names(out) = 'grade_hyper'
    return(out)
  }

  grade_hyper_multi = function(data, upper){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
      out_mat[row, 1] = sum(grade_vec[gl_by_id > upper])/sum(grade_vec) * 100
    }

    out = data.frame(out_mat)
    names(out) = 'grade_hyper'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    grade_hyper_multi(data, upper)
  } else {
    grade_hyper_single(data, upper)
  }
}


