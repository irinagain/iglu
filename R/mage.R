#' Calculate Mean Amplitude of Glucose Excursion (MAGE)
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
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
  # gl_by_id = as.double(data[data_$id %in% id_,]$gl)
  gl_by_id = as.double(data$gl)
  abs_diff_mean = abs(gl_by_id - mean(gl_by_id, na.rm = T))
  mage_out = mean(abs_diff_mean[abs_diff_mean > sd * sd(gl_by_id, na.rm = T)])
  return(mage_out)
}

