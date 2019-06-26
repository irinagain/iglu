#' Calculate percentage above targeted values
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' @param targets List of target values. Default list is (140,180,200,250).
#'
#' @examples
#' above_percent(data)
#' above_percent(data, targets = c(100, 150, 180))

above_percent <- function(data, targets = c(140,180,200,250)){
  gl_by_id = as.double(data$gl)
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
