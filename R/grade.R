#' Calculate mean GRADE score
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @return
#'
#' @export
#'
#' @examples
#' grade(example_data_5_subject)

grade <- function(data){
  grade_single = function(data){
    gl_by_id = na.omit(read_df_or_vec(data))
    grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
    out = mean(grade_vec, na.rm = T)
    out = data.frame(out)
    names(out) = 'grade'
    return(out)
  }

  grade_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
      out_mat[row, 1] = mean(grade_vec, na.rm = T)
    }

    out = data.frame(out_mat)
    names(out) = 'grade'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    grade_multi(data)
  } else {
    grade_single(data)
  }

}


