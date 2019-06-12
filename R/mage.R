#' Calculate MAGE (Mean amplitude of glucose excursion)
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' @param id Desired value(s) from id column. Default is all id's.
#' @param sd Standard deviations used to classify "excursions". Default is 1.
#' @examples
#' mage(mydata)
#' mage(mydata, 'Subject 1', 2)
#'

mage <- function(data_, id_ = unique(data_$id), sd_ = 1){
  gl_by_id = as.double(data_[data_$id %in% id_,]$gl)
  abs_diff_mean = abs(gl_by_id - mean(gl_by_id, na.rm = T))
  mage_out = mean(abs_diff_mean[abs_diff_mean > sd_ * sd(gl_by_id, na.rm = T)])
  return(mage_out)
}

