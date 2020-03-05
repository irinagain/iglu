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
#' or numeric vector of glucose values.
#'
#' @return A data.frame of values of ADRR
#'
#' @export
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' ADRR is calculated by \eqn{1/M * \sum [LR^i + HR^i]}
#' where M is number of days, LR is the max low risk value for day i
#' and HR is the max high risk value for day i. If there are no low/high risk values
#' in a day, zero is used as the corresponding LR/HR value for that day.
#'
#' Wrapping as.numeric() around the adrr call on a dataset with
#' a single subject will return a numeric value corresponding
#' to the ADRR value. This will not work for datasets with multiple subjects.
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
  adrr_single = function(subj) {
    dates = as.Date(subj$time,format="%Y-%m-%d")
    index = function(x){
      ((log(x))^1.084) - 5.381
    }
    get_extrema = function(day) {
      thisday = subj$gl[dates==day]
      HBGImax = max(index(max(thisday)),0)
      LBGImin = min(index(min(thisday)),0)
      return(22.77*((HBGImax^2)+(LBGImin^2)))
    }
    out = mean(sapply(unique(dates),FUN=get_extrema))
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

  if (class(data) == 'data.frame' && nrow(data) != 1){
    out = adrr_multi(data)
  } else{
    stop("Data must be in a data.frame structure
         with columns for 'id', 'time', and 'gl'")
  }
  return(out)
}
