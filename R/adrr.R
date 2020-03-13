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
  adrr_multi = function(data) {
    data$date = as.Date(data$time,format="%Y-%m-%d")
    out = data %>%
      dplyr::group_by(id, date) %>%
      dplyr::mutate(
        bgi = (((log(gl))^1.084) - 5.381),
        max = 22.77*(max(bgi,0)^2),
        min = 22.77*(min(bgi,0)^2)
      ) %>%
      dplyr::summarise(drr=mean(min+max)) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(adrr =mean(drr))
    return(data.frame(out))
  }

  if (class(data) == 'data.frame'){
    out = adrr_multi(data)
  } else{
    stop("Data must be in a data.frame structure
         with columns for 'id', 'time', and 'gl'")
  }
  return(out)
}
