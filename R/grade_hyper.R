#' Percentage of GRADE score above target
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @param upper Upper bound used for hyperglycemia cutoff, in mg/dL. Default is 140.
#'
#' @return
#'
#' @export
#'
#' @examples
#' grade_hyper(data)
#' grade_hyper(data, upper = 160)
grade_hyper <- function(data, upper = 140){
  gl_by_id = read_df_or_vec(data)
  #unit_constant = unit * 1 + (1-unit) * 18
  grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
  out = sum(grade_vec[gl_by_id > upper])/sum(grade_vec) * 100
  return(out)
}


# #' @param unit 0 represents mg/dL as the measurement unit,
# #' 1 represents mmol/L. Default is 0 (mg/dL).
