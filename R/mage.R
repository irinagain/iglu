#' Calculate Mean Amplitude of Glucose Excursion (MAGE)
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param sd Standard deviations used to classify "excursions". Default is 1.
#'
#' @return
#'
#' @export
#'
#' @examples
#' mage(data)
#' mage(data, 'Subject 1', 2)
#'

mage <- function(data, sd = 1){
  mage_single = function(data, sd){
    gl_by_id = na.omit(read_df_or_vec(data))
    abs_diff_mean = abs(gl_by_id - mean(gl_by_id, na.rm = T))
    out = mean(abs_diff_mean[abs_diff_mean > sd * sd(gl_by_id, na.rm = T)])
    out = data.frame(out)
    names(out) = 'mage'
    return(out)
  }

  mage_multi = function(data, sd){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      gl_by_id = na.omit(read_df_or_vec(data[data$id == subjects[row], 'gl']))
      abs_diff_mean = abs(gl_by_id - mean(gl_by_id, na.rm = T))
      out_mat[row, 1] =  mean(abs_diff_mean[abs_diff_mean > sd *
                                              sd(gl_by_id, na.rm = T)])
    }

    out = data.frame(out_mat)
    names(out) = 'mage'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    mage_multi(data, sd)
  } else {
    mage_single(data, sd)
  }

}

