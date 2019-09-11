#' Calculate percentage of values above target thresholds
#'
#' @description
#' The function above_percent produces a dataframe of values equal to
#' the percentage of glucose measurements above target values. The output
#' columns correspond to the target values. The output rows correspond to
#' the subject id's if multiple subjects are passed. The values will be between
#' 0 (no measurements) and 100 (all measurements).
#'
#' @usage
#' above_percent(data, targets = c(140,180,200,250))
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of percent.
#'
#' @param targets Numeric vector of glucose thresholds. Glucose values from
#' data argument will be compared to each value in the targets vector.
#' Default list is (140,180,200,250).
#'
#' @details
#' A dataframe structure with 1 row for each subject and a
#' column for each target value is returned.
#'
#' Wrapping as.numeric() around the above_percent call on a dataset with
#' a single subject will return a numeric vector, where the values
#' correspond to the percent of glucose values above each threshold
#' in the order passed in the targets argument. This will not work for
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
#' above_percent(example_data_1_subject) # above 140,180,200,250 by default
#' above_percent(example_data_1_subject, targets = c(100, 150, 180))
#'
#' ### output numeric vector instead of dataframe
#' as.numeric(above_percent(example_data_1_subject))
#'
#' data(example_data_5_subject)
#' above_percent(example_data_5_subject) # above 140,180,200,250 by default
#' above_percent(example_data_5_subject, targets = c(70, 170))
#'

above_percent <- function(data, targets = c(140,180,200,250)){

  gl_by_id = read_df_or_vec(data)
  targets = as.double(targets)
  out_vec = NULL
  names_list = NULL
  for(target_val in targets){
    percent = sum(gl_by_id > target_val)/length(gl_by_id) * 100
    out_vec = c(out_vec, percent)
    name = paste('above_', target_val, sep = '')
    names_list = c(names_list, name)
  }
  out = data.frame(matrix(out_vec, nrow = 1))
  names(out) = names_list
  return(out)
}

