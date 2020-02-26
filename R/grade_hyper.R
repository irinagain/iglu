#' Percentage of GRADE score attributable to hyperglycemia
#'
#' @description
#' The function grade_hyper produces \%GRADE hyperglycemia values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' grade_hyper(data, upper = 140)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of \%GRADE hyperglycemia.
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' \%GRADE hyperglycemia is calculated by calculating the GRADE score (see grade
#' function) just for values above the hyperglycemia cutoff and dividing by the
#' total GRADE score.
#'
#' Wrapping as.numeric() around the grade_hyper call on a dataset with
#' a single subject will return a numeric value corresponding to the \%GRADE
#' hyperglycemia value. This will not work for datasets with multiple subjects.
#'
#' @param upper Upper bound used for hyperglycemia cutoff, in mg/dL. Default is 140.
#'
#' @return
#'
#' @export
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
#' grade_hyper(example_data_1_subject)
#' grade_hyper(example_data_1_subject, upper = 180)
#'
#' data(example_data_5_subject)
#' grade_hyper(example_data_5_subject)
#' grade_hyper(example_data_5_subject, upper = 160)
#'

grade_hyper <- function(data, upper = 140){
  grade_hyper_single = function(data, upper){
    gl_by_id = na.omit(read_df_or_vec(data))
    grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
    grade_vec[grade_vec > 50] = 50
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
      grade_vec[grade_vec > 50] = 50
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


