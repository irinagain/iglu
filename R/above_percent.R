#' Calculate percentage of values above target thresholds
#'
#' @description
#' The function above_percent produces a dataframe of values equal to
#' the percentage of glucose measurements above target values. The output is in
#' data.frame form by default, where the columns correspond to the target
#' values and the output rows correspond to the subjects. The values will be
#' between 0 (no measurements) and 100 (all measurements).
#'
#' @usage
#' above_percent(data, targets_above = c(140, 180, 200, 250))
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of percent.
#'
#' @param targets_above Numeric vector of glucose thresholds. Glucose values from
#' data argument will be compared to each value in the targets_above vector.
#' Default list is (140, 180, 200, 250).
#'
#' @details
#' A dataframe structure with 1 row for each subject and a
#' column for each target value is returned.
#'
#' Wrapping as.numeric() around the above_percent call on a dataset with
#' a single subject will return a numeric vector, where the values
#' correspond to the percent of glucose values above each threshold
#' in the order passed in the targets_above argument. This will not work for
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
#' above_percent(example_data_1_subject)
#' above_percent(example_data_1_subject, targets_above = c(100, 150, 180))
#'
#' # output numeric vector instead of dataframe
#' as.numeric(above_percent(example_data_1_subject))
#'
#' data(example_data_5_subject)
#'
#' above_percent(example_data_5_subject)
#' above_percent(example_data_5_subject, targets_above = c(70, 170))
#'

above_percent <- function(data, targets_above = c(140,180,200,250)){
  above_percent_single = function(data, targets_above){
    gl_by_id = na.omit(read_df_or_vec(data))
    targets_above = as.double(targets_above)
    nt = length(targets_above)
    out_vec = rep(NA, nt)
    colnames_list = rep(NA, nt)
    for(target_val in targets_above){
      percent = sum(gl_by_id > target_val)/length(gl_by_id) * 100
      out_vec[targets_above == target_val] = percent
      name = paste('above_', target_val, sep = '')
      colnames_list[targets_above == target_val] = name
    }
    out = data.frame(matrix(out_vec, nrow = 1))
    names(out) = colnames_list
    return(out)
  }

  above_percent_multi = function(data, targets_above){
    subjects = unique(data$id)
    targets_above = as.double(targets_above)
    colnames_list = vector(length = length(targets_above))
    out_mat = matrix(nrow = length(subjects), ncol = length(targets_above))
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row],
                                            'gl']))
      for(col in 1:length(targets_above)){
        percent = sum(gl_by_id > targets_above[col])/length(gl_by_id) * 100
        out_mat[row, col] = percent
      }
    }
    for(col in 1:length(targets_above)){
      name = paste('above_', targets_above[col], sep = '')
      colnames_list[col] = name
    }

    out = data.frame(out_mat)
    names(out) = colnames_list
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    above_percent_multi(data,targets_above)
  } else above_percent_single(data,targets_above)

}


