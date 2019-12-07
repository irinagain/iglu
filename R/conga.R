#' Calculate continuous overall net glycemic action (CONGA)
#'
#' @description
#' The function conga produces a CONGA values in data.frame form
#' with one column and one row per subject. conga currently only supports
#' calculation of CONGA24.
#'
#' @usage
#' conga(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. Missing values will be linearly
#' interpolated when close enough to non-missing values.
#'
#' @return
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' CONGA24 is currently the only supported CONGA type.
#'
#' Wrapping as.numeric() around the above_percent call on a dataset with
#' a single subject will return a numeric vector, where the values
#' correspond to the percent of glucose values above each threshold
#' in the order passed in the targets argument. This will not work for
#' datasets with multiple subjects.
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11 Suppl 1},
#' S55-67. \doi{10.1089/dia.2008.0132}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' conga(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' conga(example_data_5_subject)
#'



conga <- function(data){
  conga_single = function(data){
    data_ip = CGMS2DayByDay(data)
    gl_by_id_ip = data_ip[[1]]

    out = sd(diff(gl_by_id_ip), na.rm = T)
    out = data.frame(out)
    names(out) = 'conga'
    return(out)
  }

  conga_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      data_by_id = data[data$id == subjects[row],]
      out_mat[row, 1] = as.numeric(conga_single(data_by_id))
    }
    out = data.frame(out_mat)
    names(out) = 'conga'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame'){
    conga_multi(data)
  } else{
    stop("Data must be in a data.frame structure
         with columns for 'id', 'time', and 'gl'")
  }
}
