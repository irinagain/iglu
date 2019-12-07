#' Calculate Coefficient of Variation (CV) of glucose levels
#'
#' @description
#' The function cv_glu produces CV values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' cv_glu(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. Missing values will be linearly
#' interpolated when close enough to non-missing values.
#'
#' @return
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' CV (Coefficient of Variation) is calculated by \eqn{100 * sd(BG) / mean(BG)}
#' Where BG is the list of all Blood Glucose measurements for a subject.
#'
#' Wrapping as.numeric() around the cv_glu call on a dataset with
#' a single subject will return a numeric value corresponding
#' to the CV value. This will not work for datasets with multiple subjects.
#'
#' @references
#' Kovatchev et al. (2006) Evaluation of a New Measure of Blood Glucose Variability in,
#' Diabetes
#' \emph{Diabetes care} \strong{29} .2433-2438,
#' \doi{10.2337/dc06-1085}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' cv_glu(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' cv_glu(example_data_5_subject)
#'

cv_glu <- function(data){
  cv_glu_single = function(data){
    gl_by_id = na.omit(read_df_or_vec(data))
    out = sd(gl_by_id, na.rm = T) / mean(gl_by_id, na.rm = T) * 100
    out = data.frame(out)
    names(out) = 'cv'
    return(out)
  }

  cv_glu_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = sd(gl_by_id, na.rm = T) / mean(gl_by_id,
                                                       na.rm = T) * 100
    }

    out = data.frame(out_mat)
    names(out) = 'cv'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    cv_glu_multi(data)
  } else {
    cv_glu_single(data)
  }

}
