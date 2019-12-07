#' Percentage of GRADE score in target range
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param lower Lower bound used for hypoglycemia cutoff, in mg/dL. Default is 80
#' @param upper Upper bound used for hyperglycemia cutoff, in mg/dL. Default is 140.
#'
#' @return
#'
#' @export
#'
#' @examples
#' grade_eugly(example_data_5_subject)
#' grade_eugly(example_data_5_subject, lower = 70, upper = 180)

grade_eugly <- function(data, lower = 80, upper = 140){
  grade_eugly_single = function(data, lower, upper){
   gl_by_id = na.omit(read_df_or_vec(data))
    grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
    out = sum(grade_vec[gl_by_id >= lower
                        & gl_by_id <= upper ])/sum(grade_vec) * 100
    out = data.frame(out)
    names(out) = 'grade_eugly'
    return(out)
  }

  grade_eugly_multi = function(data, lower, upper){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
      out_mat[row, 1] = sum(grade_vec[gl_by_id >= lower
                          & gl_by_id <= upper ])/sum(grade_vec) * 100
    }

    out = data.frame(out_mat)
    names(out) = 'grade_eugly'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    grade_eugly_multi(data, lower, upper)
  } else {
    grade_eugly_single(data, lower, upper)
  }
}

