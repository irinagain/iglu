#' Calculate J-index
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' 1 represents mmol/L. Default is 0 (mg/dL).
#'
#' @return
#'
#' @export
#'
#' @description The J-index is calculated differently for each of the
#' two measurement units. For mg/dL (unit = 0), J-index = 0.001 * (mean + sd)^2
#' For mmol/L (unit = 1), J-index = 0.324 * (mean + sd)^2
#' @examples
#' j_index(data)

j_index <- function(data){
  gl_by_id = as.double(data$gl)
  #unit_constant = unit * 0.324 + (1-unit) * 0.001
  out = .001 * (mean(gl_by_id, na.rm = T) + sd(gl_by_id, na.rm = T))^2
  return(out)
}

# #' @param unit 0 represents mg/dL as the measurement unit,
