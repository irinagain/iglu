#' Calculate percentage in targeted value ranges
#'
#' @description
#' The function in_range_percent produces a dataframe of values equal to
#' the percentage of glucose measurements in ranges of target values. The
#' output columns correspond to the target value ranges, and the rows
#' correspond to the subjects. The values will be between 0 (no measurements)
#' and 100 (all measurements).
#'
#' @usage
#' in_range_percent(data, targets = list(c(80, 200), c(70, 180), c(70, 140)))
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of percent.
#'
#' @param targets List of target value ranges wrapped in an r 'list' structure.
#'  Default list of ranges is ((80, 200), (70, 180), (70, 140)).
#'
#' @details
#' A dataframe structure with 1 row for each subject and a
#' column for each range of target values is returned.
#'
#' in_range_percent will only work properly if the targets argument is a list
#' of paired values in the format list(c(a1,b1), c(a2,b2), ...). The paired
#' values can be ordered (min, max) or (max, min). See the Examples section
#' for proper usage.
#'
#' Wrapping as.numeric() around the in_range_percent call on a dataset with
#' a single subject will return a numeric vector, where the values
#' correspond to the percent of glucose values in each range
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
#' in_range_percent(example_data_1_subject)
#' in_range_percent(example_data_1_subject, targets = list(c(50, 100), c(200,
#' 300), c(80, 140))
#'
#' # output numeric vector instead of dataframe
#' as.numeric(in_range_percent(example_data_1_subject))
#'
#' data(example_data_5_subject)
#'
#' in_range_percent(example_data_5_subject)
#' in_range_percent(example_data_1_subject, targets = list(c(60, 120), c(140,
#' 250))
#'

in_range_percent <- function(data,
                  targets = list(c(80,200), c(70,180), c(70,140))){
  in_range_percent_single = function(data, targets){
    gl_by_id = na.omit(read_df_or_vec(data))
    out_vec = NULL
    colnames_list = NULL
    for(target_range in targets){
      target_range = as.double(target_range)
      percent = sum(gl_by_id >= min(target_range) &
                    gl_by_id <= max(target_range))/length(gl_by_id) * 100
      out_vec = c(out_vec, percent)
      name = paste('in_range_', min(target_range),'_',
                  max(target_range), sep = '')
      colnames_list = c(colnames_list, name)
    }
    out = data.frame(matrix(out_vec, nrow = 1))
    names(out) = colnames_list
    return(out)
  }

  in_range_percent_multi = function(data, targets){
    subjects = unique(data$id)
    colnames_list = vector(length = length(targets))
    out_mat = matrix(nrow = length(subjects), ncol = length(targets))
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      for(col in 1:length(targets)){
        target_range = targets[[col]]
        percent = sum(gl_by_id >= min(target_range) &
                        gl_by_id <= max(target_range))/length(gl_by_id) * 100
        out_mat[row, col] = percent
      }
    }
    for(col in 1:length(targets)){
      target_range = targets[[col]]
      name = paste('in_range_', min(target_range),'_',
                   max(target_range), sep = '')
      colnames_list[col] = name
    }

    out = data.frame(out_mat)
    names(out) = colnames_list
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data)){
    in_range_percent_multi(data,targets)
  } else in_range_percent_single(data,targets)

}
