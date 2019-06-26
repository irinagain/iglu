#' Calculate hypoglycaemic index (HGI)
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' @param lower Lower bound used for hypoglycemia cutoff. Default is 80.
#'
#'
#' @examples
#' hypo_index(data)
#' hypo_index(data, upper = 70)

hypo_index <- function(data, lower = 80){
  gl_by_id = as.double(data$gl)
  out = sum(gl_by_id[gl_by_id < lower] ^ 2, na.rm = T)/length(gl_by_id * 30)
  return(out)
}



