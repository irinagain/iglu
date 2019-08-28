#' Calculate mean GRADE score
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @return
#'
#' @export
#'
#' @examples
#' grade(data)

grade <- function(data){
  gl_by_id = read_df_or_vec(data)
  #unit_constant = unit * 1 + (1-unit) * 18
  grade_vec = 425*(log10(log10(gl_by_id/18))+0.16)^2
  out = mean(grade_vec, na.rm = T)
  out = data.frame(out)
  names(out) = 'grade'
  return(out)
}


# #' @param unit 0 represents mg/dL as the measurement unit,
