#' Calculate Hypoglycemia Hndex
#'
#' @description
#' The function hypo_index produces Hypoglycemia index values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' hypo_index(data, lower = 70)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of Hypoglycemic Index.
#'
#' @param lower Lower bound used for hypoglycemia cutoff. Default is 70
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' Hypoglycemia Index is calculated by \eqn{n/30 * \sum [hypoBG_j ^{2}]}
#' Where n is the total number of Blood Glucose measurements and \eqn{hypoBG_j}
#' is the jth Blood Glucose measurement below the hypoglycemia cutoff.
#'
#' Wrapping as.numeric() around the hypo_index call on a dataset with
#' a single subject will return a numeric value corresponding to the
#' Hypoglycemia Index value. This will not work for datasets with multiple subjects.
#'
#' @return
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#' hypo_index(example_data_1_subject)
#' hypo_index(example_data_1_subject, lower = 60)
#'
#' data(example_data_5_subject)
#' hypo_index(example_data_5_subject)
#' hypo_index(example_data_5_subject, lower = 80)
#'

hypo_index <- function(data, lower = 70){
  hypo_index_single = function(data, lower){
    gl_by_id = na.omit(read_df_or_vec(data))
    out = sum(gl_by_id[gl_by_id < lower] ^ 2, na.rm = T)/(length(gl_by_id) * 30)
    out = data.frame(out)
    names(out) = 'hypo_index'
    return(out)
  }
  hypo_index_multi = function(data, lower){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = sum(gl_by_id[gl_by_id < lower] ^ 1.1, na.rm = T)/
        (length(gl_by_id) * 30)
    }

    out = data.frame(out_mat)
    names(out) = 'hypo_index'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    hypo_index_multi(data, lower)
  } else {
    hypo_index_single(data, lower)
  }
}
