#' Calculate percentage in targeted value ranges
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param targets List of target values. Default list is [(80,200), (70,180), (70,140)].
#'
#' @return
#'
#' @export
#'
#' @examples
#' in_range_percent(data)
#' in_range_percent(data, targets = list(c(100,200), c(150,250))
#'
#' # The following will return same output as previous output:
#' in_range_percent(data, targets = list(c(200, 100), c(250,150))

in_range_percent <- function(data,
                  targets = list(c(80,200), c(70,180), c(70,140))){
  gl_by_id = read_df_or_vec(data)
  out_vec = NULL
  names_list = NULL
  for(target_range in targets){
    target_range = as.double(target_range)
    percent = sum(gl_by_id >= min(target_range) &
                  gl_by_id <= max(target_range))/length(gl_by_id) * 100
    out_vec = c(out_vec, percent)
    name = paste('in_range_', min(target_range),'_',
                 max(target_range), sep = '')
    names_list = c(names_list, name)
  }
  out = data.frame(matrix(out_vec, nrow = 1))
  names(out) = names_list
  return(out)
}
