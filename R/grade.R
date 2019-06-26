#' Calculate mean GRADE score
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' 1 represents mmol/L. Default is 0 (mg/dL).
#'
#' @return
#'
#' @export
#'
#' @examples
#' grade(data)

grade <- function(data){
  gl_by_id = as.double(data$gl)
  #unit_constant = unit * 1 + (1-unit) * 18
  grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
  out = mean(grade_vec, na.rm = T)
  return(out)
}


# #' @param unit 0 represents mg/dL as the measurement unit,
