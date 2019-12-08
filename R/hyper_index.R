#' Calculate Hyperglycemia Hndex (HGI)
#'
#' @description
#' The function hyper_index produces Hyperglycemia index values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' hyper_index(data, upper = 140)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of Hyperglycemic Index.
#'
#' @param upper Upper bound used for hyperglycemia cutoff. Default is 140.
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' Hyperglycemia Inndex is calculated by \eqn{n/30 * \sum [hyperBG_j ^{1.1}]}
#' Where n is the total number of Blood Glucose measurements and \eqn{hyperBG_j}
#' is the jth Blood Glucose measurement above the hyperglycemia cutoff.
#'
#' Wrapping as.numeric() around the hyper_index call on a dataset with
#' a single subject will return a numeric value corresponding to the
#' Hyperglycemia Index value. This will not work for datasets with multiple subjects.
#'
#' @return
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#' hyper_index(example_data_1_subject)
#' hyper_index(example_data_1_subject, upper = 160)
#'
#' data(example_data_5_subject)
#' hyper_index(example_data_5_subject)
#' hyper_index(example_data_5_subject, upper = 150)


hyper_index <- function(data, upper = 140){
  hyper_index_single = function(data, upper){
    gl_by_id = na.omit(read_df_or_vec(data))
    out = sum(gl_by_id[gl_by_id > upper] ^ 1.1, na.rm = T)/(length(gl_by_id) * 30)
    out = data.frame(out)
    names(out) = 'hyper_index'
    return(out)
  }

  hyper_index_multi = function(data, upper){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      out_mat[row, 1] = sum(gl_by_id[gl_by_id > upper] ^ 1.1, na.rm = T)/
                                                  (length(gl_by_id) * 30)
    }

    out = data.frame(out_mat)
    names(out) = 'hyper_index'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    hyper_index_multi(data, upper)
  } else {
    hyper_index_single(data, upper)
  }
}
