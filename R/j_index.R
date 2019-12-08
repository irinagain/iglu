#' Calculate J-index
#'
#' @description
#' The function j_index produces J-index values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' j_index(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of J-index score.
#'
#'
#' @return
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' J-index score is calculated by \eqn{.001 * [mean(BG) + sd(BG)]^2}
#' where BG is the list of Blood Glucose Measurements.
#'
#' Wrapping as.numeric() around the j_index call on a dataset with
#' a single subject will return a numeric value corresponding to the J-index
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
#' j_index(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' j_index(example_data_5_subject)
#'

j_index <- function(data){
  j_index_single = function(data){
    gl_by_id = na.omit(read_df_or_vec(data))
    out = .001 * (mean(gl_by_id, na.rm = T) + sd(gl_by_id, na.rm = T))^2
    out = data.frame(out)
    names(out) = 'j_index'
    return(out)
  }

  j_index_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = .001 * (mean(gl_by_id, na.rm = T) +
                                  sd(gl_by_id, na.rm = T))^2
    }

    out = data.frame(out_mat)
    names(out) = 'j_index'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    j_index_multi(data)
  } else {
    j_index_single(data)
  }

}

