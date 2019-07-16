#' Calculate percentage below targeted values
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param targets List of target values. Default list is (50,80).
#'
#' @return
#'
#' @export
#'
#' @examples
#' below_percent(data)
#' below_percent(data, targets = c(50,100, 180))

below_percent <- function(data, targets = c(50,80)){
  gl_by_id = read_df_or_vec(data)
  targets = as.double(targets)
  out_vec = NULL
  names_list = NULL
  for(target_val in targets){
    percent = sum(gl_by_id < target_val)/length(gl_by_id) * 100
    out_vec = c(out_vec, percent)
    name = paste('below_', target_val, sep = '')
    names_list = c(names_list, name)
  }
  out = data.frame(matrix(out_vec, nrow = 1))
  names(out) = names_list
  return(out)
}
