#' Calculate hyperglycaemic index (HGI)
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' @param upper Upper bound used for hyperglycemia cutoff. Default is 140.
#'
#' @return
#'
#' @export
#'
#' @examples
#' hyper_index(data)
#' hyper_index(data, upper = 160)

hyper_index <- function(data, upper = 140){
  gl_by_id = as.double(data$gl)
  out = sum(gl_by_id[gl_by_id > upper] ^ 1.1, na.rm = T)/length(gl_by_id * 30)
  return(out)
}



