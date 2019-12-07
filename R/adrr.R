#' Calculate average daily risk range (ADRR)
#'
#'
#' @description
#' The function adrr produces ADRR values in data.frame form
#' with one column and one row per subject.
#'
#' @usage
#' adrr(data)
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
#' ADRR is calculated by \eqn{1/M * \sum [LR^i + HR^i]}
#' where M is number of days, LR is the max low risk value for day i
#' and HR is the max high risk value for day i.
#'
#' Wrapping as.numeric() around the above_percent call on a dataset with
#' a single subject will return a numeric vector, where the values
#' correspond to the percent of glucose values above each threshold
#' in the order passed in the targets argument. This will not work for
#' datasets with multiple subjects.
#'
#' @references
#' Kovatchev et al. (2006) Evaluation of a New Measure of Blood Glucose Variability in,
#' Diabetes
#' \emph{Diabetes care} \strong{29} .2433-2438,
#' \doi{10.2337/dc06-1085}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' adrr(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' adrr(example_data_5_subject)
#'


adrr <- function(data){
  adrr_single = function(data){
    data_ip = CGMS2DayByDay(data)
    gl_by_id_ip = data_ip[[1]]

    fBG = 1.509*(log(gl_by_id_ip)^1.084 - 5.381)
    rBG = 10*fBG^2

    rlbg = matrix(0, nrow = nrow(gl_by_id_ip), ncol = ncol(gl_by_id_ip))
    rhbg = matrix(0, nrow = nrow(gl_by_id_ip), ncol = ncol(gl_by_id_ip))

    rlbg[which(fBG<0)] = rBG[which(fBG<0)]
    rhbg[which(fBG>0)] = rBG[which(fBG>0)]

    out = mean(apply(rlbg,1,max) + apply(rhbg,1,max))
    out = data.frame(out)
    names(out) = 'adrr'
    return(out)
  }

  adrr_multi = function(data){
    subjects = unique(data$id)
    out_mat = matrix(nrow = length(subjects), ncol = 1)
    for(row in 1:length(subjects)){
      data_by_id = data[data$id == subjects[row],]
      out_mat[row, 1] = as.numeric(adrr_single(data_by_id))
    }

    out = data.frame(out_mat)
    names(out) = 'adrr'
    row.names(out) = unique(subjects)
    return(out)
  }

  if(class(data) == 'data.frame' && nrow(data) != 1){
    adrr_multi(data)
  } else{
    stop("Data must be in a data.frame structure
         with columns for 'id', 'time', and 'gl'")
  }

}
