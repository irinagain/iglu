#' Calculate Mean Amplitude of Glycemic Excursions
#'
#' @description
#' The function mage produces MAGE values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' mage(data, sd_multiplier = 1)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of MAGE.
#'
#' @param sd_multiplier A numeric value that can change the sd value used
#' to determine size of glycemic excursions used in the calculation.
#' @return
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' MAGE is calculated by taking the mean of absolute differences (between
#' each value and the mean) that are greater than the standard deviation.
#' A multiplier can be added to the standard deviation by the sd_multiplier
#' argument.
#'
#' Wrapping as.numeric() around the mage call on a dataset with
#' a single subject will return a numeric value corresponding to the MAGE value.
#' This will not work for datasets with multiple subjects.
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
#' mage(example_data_1_subject)
#' mage(example_data_1_subject, sd_multiplier = 2)
#'
#' data(example_data_5_subject)
#' mage(example_data_5_subject, sd_multiplier = .9)
#'

mage <- function(data, sd_multiplier = 1){
  mage_single = function(data, sd_multiplier){
    gl_by_id = na.omit(read_df_or_vec(data))
    abs_diff_mean = abs(gl_by_id - mean(gl_by_id, na.rm = T))
    out = mean(abs_diff_mean[abs_diff_mean > sd_multiplier * sd(gl_by_id, na.rm = T)])
    out = data.frame(out)
    names(out) = 'mage'
    return(out)
  }

  mage_multi = function(data, sd_multiplier){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      abs_diff_mean = abs(gl_by_id - mean(gl_by_id, na.rm = T))
      out_mat[row, 1] =  mean(abs_diff_mean[abs_diff_mean > sd_multiplier *
                                              sd(gl_by_id, na.rm = T)])
    }

    out = data.frame(out_mat)
    names(out) = 'mage'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    mage_multi(data, sd_multiplier)
  } else {
    mage_single(data, sd_multiplier)
  }

}

