#' Calculate continuous overall net glycemic action (CONGA)
#'
#' @description
#' The function conga produces a CONGA values a tibble object.
#' This version is a prototype which allows setting the gap
#'
#' @usage
#' conga(data, tz = "", gap = 1)
#'
#' @param data DataFrame object with column names "id", "time", and "gl".
#'
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning.
#'
#' @return  A tibble object with two columns: subject id and corresponding CONGA value.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the CONGA values is returned.
#'
#' Missing values will be linearly interpolated when close enough to non-missing values.
#'
#' CONGA_n is the standard deviation of the difference between glucose values that are exactly n hours apart.
#'
#'
#' @references
#' McDonnell et al. (2005) : A novel approach to continuous glucose analysis
#' utilizing glycemic variation
#' \emph{Diabetes Technology and Therapeutics} \strong{7} .253-263,
#' \doi{10.1089/dia.2005.7.253}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' conga(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' conga(example_data_5_subject)
#'



conga <- function(data, tz = "", n = 1){
  conga_single = function(data, tz = "", gap = 1){
    data_ip = CGMS2DayByDay(data, tz = tz)
    gl_by_id_ip = data_ip[[1]]

    out = sd(diff(t(gl_by_id_ip), lag = 12*gap), na.rm = TRUE)
    return(out)
  }

  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  data$date = as.Date(data$time,format="%Y-%m-%d")
  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id, date) %>%
    dplyr::summarise(
      conga_day = conga_single(data.frame(id, time, gl), tz = tz, gap = n)
    ) %>%
    dplyr::summarise(conga = mean(conga_day))
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
