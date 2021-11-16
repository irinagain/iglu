#' Calculate average daily risk range (ADRR)
#'
#' @description
#' The function adrr produces ADRR values in a tibble object.
#'
#' @usage
#' adrr(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl".
#'
#' @return A tibble object with two columns: subject id and corresponding
#' ADRR value.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for ADRR values is returned. NA glucose values are
#' omitted from the calculation of the ADRR values.
#'
#' ADRR is the average sum of HBGI corresponding to the highest glucose
#' value and LBGI corresponding to the lowest glucose value for each day,
#' with the average taken over the daily sums. If there are no high glucose or
#' no low glucose values, then 0 will be substituted for the HBGI value or the
#' LBGI value, respectively, for that day.
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
    bgi = drr = id = NULL
    rm(list = c("id", "bgi", "drr"))
    data$date = as.Date(data$time,format="%Y-%m-%d")
    out = data %>%
      dplyr::filter(!is.na(gl)) %>%
      dplyr::group_by(id, date) %>%
      dplyr::mutate(
        bgi = ((log(gl)^1.084) - 5.381),
        max = 22.77*(max(bgi,0)^2),
        min = 22.77*(min(bgi,0)^2)
      ) %>%
      dplyr::summarise(drr = mean(min+max)) %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(ADRR = mean(drr))
    return(out)
  }

  if ('data.frame' %in% class(data) && nrow(data) != 1){
    out = adrr_multi(data)
  } else{
    stop("Data must be in a data.frame structure
         with columns for 'id', 'time', and 'gl'")
  }
  return(out)
}
