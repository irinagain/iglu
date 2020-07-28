#' Calculate percentage of time CGM was active
#'
#' @description
#' The function active_percent produces a tibble object with values equal to the
#' percentage of time the cgm was active. For example, if a cgm's (5 min frequency) times were 0, 5, 10, 15 and
#' glucose values were missing at time 5, then percentage of time the cgm was active is 75%.
#' The output columns correspond to the subject id and the precentage of time for which the cgm was active,
#' and the output rows correspond to the subjects.
#' The values will be between 0% (no measurements) and 100% (all measurements).
#'
#' @usage
#' active_percent(data, freqCGM = 5)
#'
#' @param data DataFrame object with column names "id", "time", and "gl"
#' @param freqCGM Numeric value of CGM Frequency(In minutes). Default value is 5.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for active_percent values is returned.
#'
#' @return If a data.frame object is passed, then a tibble object with two columns: subject id and
#' corresponding active_percent value is returned
#'
#' @export
#'
#' @author Pratik Patel
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
#' active_percent(example_data_1_subject, freqCGM = 15)
#'
#' data(example_data_5_subject)
#'
#' active_percent(example_data_5_subject)
#' active_percent(example_data_5_subject, freqCGM = 5)
#'


active_percent <- function(data, freqCGM = 5) {
  active_percent = gl = id = ns = NULL
  rm(list = c("gl", "id", "active_percent", "ns"))
  data = check_data_columns(data, time_check = TRUE)
  is_vector = attr(data, "is_vector")

  subject = unique(data$id)
  ns = length(subject)

  active_perc_data = list()
  for(i in 1:ns) {
    subData <- data %>%
      dplyr::filter(!is.na(gl)) %>%
      dplyr::filter(!is.na(time)) %>%
      dplyr::filter(id == subject[i]) %>%
      dplyr::arrange(time)

    present_gl_vals <- nrow(subData)
    theoretical_gl_vals <- 0
    start_time <- subData$time[1]
    for(j in 1:(present_gl_vals-1)) {
      if(as.double(subData$time[j+1]) - as.double(subData$time[j]) >= (24*3600)) {
        n_gl_vals <- round((as.double(subData$time[j]) - as.double(start_time))/(60*freqCGM)) + 1
        theoretical_gl_vals = theoretical_gl_vals + n_gl_vals
        start_time <- subData$time[j+1]
      }
    }
    n_gl_vals <- round(((as.double(subData$time[present_gl_vals]) - as.double(start_time))/(60*freqCGM))) + 1
    theoretical_gl_vals = theoretical_gl_vals + n_gl_vals
    active_perc_data[[i]] <- c(present_gl_vals, theoretical_gl_vals)
  }

  results <- lapply(
    active_perc_data,
    function(d){
      out = tibble::tibble(id = NA, active_percent = (d[1]/d[2])*100)
      out
    }
  )

  results = dplyr::bind_rows(results)
  results$id = subject

  if (is_vector) {
    results$id = NULL
  }
  return(results)
}
