#' Calculate SD subtypes
#'
#' @description
#' The function \code{sd_measures} produces SD subtype values in a tibble object
#' with a row for each subject and columns corresponding to id followed by
#' each SD subtype.
#'
#' @usage
#' sd_measures(data,dt0 = NULL, inter_gap = 45, tz = "")
#'
#' @inheritParams conga
#' @inheritParams CGMS2DayByDay
#'
#' @return
#' A tibble object with a column for id and a column for each of the six SD subtypes.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for each SD subtype values is returned.
#'
#' Missing values will be linearly interpolated when close enough to non-missing values.
#'
#' \enumerate{
#'
#' \item SDw - vertical within days:
#'
#' Calculated by first taking the standard deviation of each day's glucose measurements,
#' then taking the mean of all the standard deviations. That is, for d
#' days we compute \eqn{SD_1 ... SD_d} daily standard deviations and calculate
#' \eqn{1/d * \sum [(SD_i)]}
#'
#' \item SDhhmm - between time points:
#'
#' Also known as SDhh:mm. Calculated by taking the mean glucose values at each time point in the grid across days,
#' and taking the standard deviation of those mans. That is, for t time points
#' we compute \eqn{X_t} means for each time point and then compute \eqn{SD([X_1, X_2, ... X_t])}.
#'
#' \item SDwsh - within series:
#'
#' Also known as SDws h. Calculated by taking the hour-long intervals starting at every point in the interpolated
#' grid, computing the standard deviation of the points in each hour-long interval, and then
#' finding the mean of those standard deviations. That is, for n time points compute
#' \eqn{SD_1 ... SD_n}, where \eqn{SD_i} is the standard deviation of the glucose values \eqn{[X_i, X_{i+1}, ... X_{i+k}]}
#' corresponding to hour-long window starting at observation \eqn{X_i}, the number of observations in the window k depends on CGM meter frequency. Then, take
#' \eqn{1/n * \sum [(SD_i)]}.
#'
#' \item SDdm - horizontal sd:
#'
#' Calculated by taking the daily mean glucose values, and then taking the standard deviation
#' of those daily means. That is, for d days we take \eqn{X_1 ... X_d} daily means, and then compute
#' \eqn{SD([X_1, X_2, ... X_d])}.
#'
#' \item SDb -  between days, within timepoints:
#'
#' Calculated by taking the standard deviation of the glucose values across days for each time point,
#' and then taking the mean of those standard deviations.
#' That is, for t time points take \eqn{SD_1 ... SD_t} standard deviations, and then compute
#' \eqn{1/t * \sum[(SD_i)]}
#'
#' \item SDbdm - between days, within timepoints, corrected for changes in daily means:
#'
#' Also known as SDb // dm. Calculated by subtracting the daily mean from each glucose value, then taking the standard deviation
#' of the corrected glucose values across days for each time point, and then taking the mean of those
#' standard deviations.
#' That is, for t time points take \eqn{SD_1 ... SD_t} standard deviations, and then compute
#' \eqn{1/t * \sum[(SD_i)]}. where \eqn{SD_i} is the standard deviation of d daily values at the
#' 1st time point, where each value is the dth measurement for the ith time point subtracted by
#' the mean of all glucose values for day d.
#'
#'
#' }
#'
#' @references
#' Rodbard (2009) New and Improved Methods to Characterize Glycemic Variability
#' Using Continuous Glucose Monitoring
#' \emph{Diabetes Technology and Therapeutics} \strong{11} .551-565,
#' \doi{10.1089/dia.2009.0015}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' sd_measures(example_data_1_subject)
#'

sd_measures <- function(data, dt0 = NULL, inter_gap = 45, tz = ""){

  id = NULL
  rm(list = c("id"))
  data = check_data_columns(data, time_check=TRUE, tz = tz)
  subject = unique(data$id)
  ns = length(subject)

  # Calculate uniform grid for all subjects
  gdall = list()
  for (i in 1:ns){
    if (i != 1){
      dt0 = out$dt0
    }
    out = data %>%
      dplyr::filter(id == subject[i]) %>%
      CGMS2DayByDay(tz = tz, dt0 = dt0, inter_gap = inter_gap)
    gdall[[i]] <- out$gd2d
  }
  dt0 = out$dt0

  results = lapply(
    gdall,
    function(gd2d) {
      # SdW - vertical within days
      out = tibble::tibble(id = NA, SDw = mean(apply(gd2d, 1, sd, na.rm = TRUE), na.rm = TRUE))
      # SDhh:mm - between time points
      out$SDhhmm= sd(apply(gd2d, 2, mean, na.rm = TRUE), na.rm = TRUE)
      # SDwsh - Within series - for 1 hour window
      win = round(60/dt0) # how many measurements are within 1 hour
      gs = as.vector(t(gd2d))
      #N = length(gs) # total # of measurements
      #ind = rep(1:ceiling(N/win), each = win)[1:N] # consecutive indexing
      #out$SdWSH = mean(tapply(gs, ind, sd, na.rm = TRUE), na.rm = TRUE)
      out$SDwsh = mean(caTools::runsd(gs, k = win, endrule = "trim"), na.rm = TRUE)

      # SDdm - "Horizontal" sd
      meanR = apply(gd2d, 1, mean, na.rm = TRUE)
      out$SDdm = sd(meanR, na.rm = TRUE)

      # SDb - between days, within timepoints
      out$SDb = mean(apply(gd2d, 2, sd, na.rm = TRUE), na.rm = TRUE)

      # SDbdm - between days, within timepoints, corrected for changes in daily means
      med = matrix(rep(meanR, each = ncol(gd2d)), ncol = ncol(gd2d), byrow = TRUE)
      # out$SdBDM = mean(apply(sqrt((gd2d - med)^2), 1, mean, na.rm = TRUE), na.rm = TRUE)
      out$SDbdm = mean(apply(gd2d - med, 2, sd, na.rm = TRUE), na.rm = TRUE)

      out
    })

    results = dplyr::bind_rows(results)
    results$id = subject

  return(results)
}
