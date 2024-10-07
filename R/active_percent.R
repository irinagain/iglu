#' Calculate percentage of time CGM was active
#'
#' @description
#' The function `active_percent` produces the % of time CGM is active together with the length of the measurement period
#'
#' @usage
#' active_percent(data, dt0 = NULL, tz = "")
#'
#' @inheritParams plot_lasagna
#'
#' @param tz \strong{tz = "".} A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning.
#'
#' @details
#'The function `active_percent` produces a tibble object with values equal to the
#' percentage of time the CGM was active, the total number of observed days, the start date, and the end date. For example, if a CGM's (5 min frequency) times were 0, 5, 10, 15 and
#' glucose values were missing at time 5, then percentage of time the CGM was active is 75%.
#' The output columns correspond to the subject id, the percentage of time for which the CGM was active, the number of days of measurements, the start date and the end date of measurements.
#' The output rows correspond to the subjects.
#' The values of `active_percent` are always between 0% (no measurements) and 100% (all measurements).
#'
#' @return A tibble object with five columns: subject id,
#' corresponding active_percent value, duration of measurement period in days, start date, and end date.
#'
#' @export
#'
#' @author Pratik Patel, Irina Gaynanova
#'
#' @references
#' Danne et al. (2017) International Consensus on Use of
#' Continuous Glucose Monitoring
#' \emph{Diabetes Care} \strong{40} .1631-1640,
#' \doi{10.2337/dc17-1600}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#'
#' active_percent(example_data_1_subject)
#'
#' data(example_data_5_subject)
#'
#' active_percent(example_data_5_subject)
#' active_percent(example_data_5_subject, dt0 = 5, tz = 'GMT')
#'


active_percent <- function(data, dt0 = NULL, tz = "", range_type = "automatic", ndays = 14, end_date = NULL) {
  active_percent = gl = id = NULL
  rm(list = c("gl", "id", "active_percent"))
  data = check_data_columns(data, time_check = TRUE, tz = tz)
  is_vector = attr(data, "is_vector")

  subject = unique(data$id)
  ns = length(subject)

  # Calculating present and theoretical number of gl values for each id
  active_perc_data = list()
  # Loop over the subjects
  for(i in 1:ns) {
    subData <- data %>%
      dplyr::filter(!is.na(gl)) %>%
      dplyr::filter(!is.na(time)) %>%
      dplyr::filter(id == subject[i]) %>%
      dplyr::arrange(time)

    #present_gl_vals = nrow(subData)
    #theoretical_gl_vals = 0
    #start_time = subData$time[1]

    timeindex = 2:nrow(subData)
    timediff = difftime(subData$time[timeindex], subData$time[timeindex - 1], units = "mins")

    ### Automatically identify grid width dt0
    if (is.null(dt0)){
      dt0 = as.double(round(median(timediff, na.rm = TRUE)))
    }

    if(range_type == "automatic"){
      # Determine range of observed data
      mintime = min(subData$time)
      maxtime = max(subData$time)
      # Determine the overall length in minutes of the observed period
      theoretical_gl_vals = round(as.numeric(round(difftime(maxtime, mintime, units = "mins")))/dt0) + 1
      # Determine the overall length in minutes of all the gaps longer than dt0 min apart
      gap_minutes = sum(as.numeric(timediff[round(timediff) > dt0]))
      ngaps = sum(round(timediff) > dt0)
      missing_gl_vals = round((gap_minutes - ngaps * dt0)/dt0)
      ndays = difftime(maxtime, mintime, units = "days")
      active_perc_data[[i]]$percent <- (theoretical_gl_vals - missing_gl_vals)/theoretical_gl_vals
      active_perc_data[[i]]$mintime <- mintime
      active_perc_data[[i]]$maxtime <- maxtime
      active_perc_data[[i]]$ndays <- ndays
    } else if(range_type == "manual"){
      #Determine range of observed data under range_type = "exact"
      if(!is.null(end_date)){
        end_date = as.POSIXct(end_date)
      } else{
        end_date = as.POSIXct(tail(subData$time, n = 1))
      }
      start_date = end_date - days(as.integer(ndays))
      date_range <- interval(start = start_date, end = end_date)
      subData <- subData %>% filter(time %within% date_range)
      active_perc_data[[i]]$percent <- (nrow(subData)/(as.numeric(ndays)*(24*(60/dt0))))
      active_perc_data[[i]]$mintime <- start_date
      active_perc_data[[i]]$maxtime <- end_date
      active_perc_data[[i]]$ndays <- difftime(end_date, start_date, units = "days")
    }
  }

  results = lapply(
    active_perc_data,
    function(d){
      out = tibble::tibble(id = d$id, active_percent = d$percent*100, ndays = round(d$ndays, 1), start_date = d$mintime, end_date = d$maxtime)
      out
    }
  )

  results = dplyr::bind_rows(results)

  if (is_vector) {
    results$id = NULL
  }
  return(results)
}

