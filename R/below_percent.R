#' Calculate percentage below targeted values
#'
#' #' @description
#' The function above_percent produces a dataframe of values equal to
#' the percentage of glucose measurements below target values. The output is in
#' data.frame form by default, where the columns correspond to the target
#' values and the output rows correspond to the subjects. The values will be
#' between 0 (no measurements) and 100 (all measurements).
#'
#' @usage
#' below_percent(data, targets_below = c(50, 80))
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double. NA's will be
#' omitted from the glucose values in calculation of percent.
#'
#' @param targets_below Numeric vector of glucose thresholds. Glucose values from
#' data argument will be compared to each value in the targets_below vector.
#' Default list is (50, 80).
#'
#' @details
#' A dataframe structure with 1 row for each subject and a
#' column for each target value is returned.
#'
#' Wrapping as.numeric() around the below_percent call on a dataset with
#' a single subject will return a numeric vector, where the values
#' correspond to the percent of glucose values below each threshold
#' in the order passed in the targets_below argument. This will not work for
#' datasets with multiple subjects.
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
#'
#' below_percent(example_data_1_subject)
#' below_percent(example_data_1_subject, targets_below = c(50, 100, 180))
#'
#' as.numeric(below_percent(example_data_1_subject))
#'
#' data(example_data_5_subject)
#'
#' below_percent(example_data_5_subject)
#' below_percent(example_data_5_subject, targets_below = c(80, 180))
#'


below_percent <- function(data, targets_below = c(50,80)){
  below_percent_single = function(data,targets_below){
    gl_by_id = na.omit(read_df_or_vec(data))
    targets_below = as.double(targets_below)
    out_vec = NULL
    colnames_list = NULL
    for(target_val in targets_below){
      percent = sum(gl_by_id < target_val)/length(gl_by_id) * 100
      out_vec = c(out_vec, percent)
      name = paste('below_', target_val, sep = '')
      colnames_list = c(colnames_list, name)
    }
    out = data.frame(matrix(out_vec, nrow = 1))
    names(out) = colnames_list
    return(out)
  }

  below_percent_multi = function(data, targets_below){
    subjects = unique(data$id)
    targets_below = as.double(targets_below)
    colnames_list = vector(length = length(targets_below))
    out_mat = matrix(nrow = length(subjects), ncol = length(targets_below))
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row],
                                             'gl']))
      for(col in 1:length(targets_below)){
        percent = sum(gl_by_id < targets_below[col])/length(gl_by_id) * 100
        out_mat[row, col] = percent
      }
    }
    for(col in 1:length(targets_below)){
      name = paste('below_', targets_below[col], sep = '')
      colnames_list[col] = name
    }

    out = data.frame(out_mat)
    names(out) = colnames_list
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    below_percent_multi(data,targets_below)
  } else below_percent_single(data,targets_below)

}




