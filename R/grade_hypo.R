#' Percentage of GRADE score below target
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' @param lower Lower bound used for hypoglycemia cutoff, in mg/dL. Default is 80
#'
#' @examples
#' grade_hypeodata)
#' grade_hypo(data, lower = 70)

grade_hyper <- function(data, lower = 80){
  gl_by_id = as.double(data$gl)
 # unit_constant = unit * 1 + (1-unit) * 18
  grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
  out = sum(grade_vec[gl_by_id < lower])/sum(grade_vec) * 100
  return(out)
}

# #' @param unit 0 represents mg/dL as the measurement unit,
# #' 1 represents mmol/L. Default is 0 (mg/dL).
