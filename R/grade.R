#' Calculate mean GRADE score
#'
#' @description
#' The function grade produces GRADE score values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' grade(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of sd.
#'
#' @return
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' GRADE score is calculated by \eqn{1/n * \sum [425 *
#' (log(log(BG_i * 18)) + .16)^2]}
#' Where \eqn{BG_i} is the ith Blood Glucose measurement and n is the total
#' number of measurements.
#'
#' Wrapping as.numeric() around the grade call on a dataset with
#' a single subject will return a numeric value corresponding to the GRADE
#' score. This will not work for datasets with multiple subjects.
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11 Suppl 1},
#' S55-67. \doi{10.1089/dia.2008.0132}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' grade(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' grade(example_data_5_subject)
#'

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


