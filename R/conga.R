#' Continuous Overall Net Glycemic Action (CONGA)
#'
#' @description
#' The function conga produces CONGA values a tibble object for any n hours apart.
#'
#' @usage
#' conga(data, n = 24, tz = "")
#'
#' @param data DataFrame object with column names "id", "time", and "gl".
#'
#' @param tz A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning.
#'
#' @param n An integer specifying how many hours prior to an observation should be used in the CONGA calculation. The default value is set to n = 24 hours
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
#' CONGA is the standard deviation of the difference between glucose values that are exactly n hours apart. CONGA is computed by taking the standard deviation of differences in measurements separated by n hours.
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


conga <- function(data, n = 24, tz = ""){
  conga_single = function(data, hours = 1, tz = ""){
    data_ip = CGMS2DayByDay(data, tz = tz)
    gl_by_id_ip = data_ip[[1]]
    dt0 = data_ip[[3]]
    hourly_readings = round(60 / dt0, digits = 0)

    out = sd(diff(as.vector(t(gl_by_id_ip)), lag = hourly_readings * hours), na.rm = TRUE)
    return(out)
  }

  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      conga = conga_single(data.frame(id, time, gl), hours = n, tz = tz)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
