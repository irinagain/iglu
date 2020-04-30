#' Calculate mean difference between glucose values obtained at the same time
#' of day (MODD)
#'
#' @description
#' The function modd produces MODD values in a tibble object.
#'
#' @usage
#' modd(data, lag = 1)
#'
#'
#' @param data DataFrame object with column names "id", "time", and "gl".
#'
#' @param lag Integer indicating which lag (# days) to use. Default is 1.
#'
#' @return A tibble object with two columns: subject id and corresponding MODD value.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the MODD values is returned.
#'
#' Missing values will be linearly interpolated when close enough to non-missing values.
#'
#' MODD is calculated by taking the mean of absolute differences between
#' measurements at the same time 1 day away, or more if lag parameter
#' is set to an integer > 1.
#'
#' @references
#' Service, Nelson (1980) Characteristics of glycemic stability.
#' \emph{Diabetes care} \strong{3} .58-62,
#' \doi{10.2337/diacare.3.1.58}.
#' @examples
#'
#' data(example_data_1_subject)
#' modd(example_data_1_subject)
#' modd(example_data_1_subject, lag = 2)
#'
#' data(example_data_5_subject)
#' modd(example_data_5_subject, lag = 2)
#'

modd <- function(data, lag = 1, tz = ""){
  modd_single = function(data, lag){
    data_ip = CGMS2DayByDay(data, tz=tz)
    gl_by_id_ip = data_ip[[1]]
    out = mean(abs(diff(gl_by_id_ip, lag = lag)), na.rm = TRUE)
    return(out)
  }

  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      modd = modd_single(data.frame(id,time,gl),lag)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
