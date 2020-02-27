#' Percentage of GRADE score attributable to hypoglycemia
#'
#' @description
#' The function grade_hypo produces \%GRADE hypoglycemia values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' grade_hypo(data, lower = 70)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of \%GRADE hypoglycemia.
#'
#' @param lower Lower bound used for hypoglycemia cutoff, in mg/dL. Default is 70
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' \%GRADE hypoglycemia is calculated by calculating the GRADE score (see grade
#' function) just for values below the hypoglycemia cutoff and dividing by the
#' total GRADE score.
#'
#' Wrapping as.numeric() around the grade_hypo call on a dataset with
#' a single subject will return a numeric value corresponding to the \%GRADE
#' hypoglycemia value. This will not work for datasets with multiple subjects.
#'
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
#' grade_hypo(example_data_1_subject)
#' grade_hypo(example_data_1_subject, lower = 80)
#'
#' data(example_data_5_subject)
#' grade_hypo(example_data_5_subject)
#' grade_hypo(example_data_5_subject, lower = 65)
#'

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

  if (class(data) == 'data.frame'){
    out = grade_hypo_multi(data, lower)
  } else {
    out = grade_hypo_single(data, lower)
  }
  return(out)
}
