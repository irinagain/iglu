#' Calculate SD subtypes
#'
#' @description
#' The function sd_measures produces SD subtype values in a tibble object
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
#' \item SdW- vertical within days:
#'
#' Calculated by first taking the standard deviation of each day's glucose measurements,
#' then taking the mean of all the standard deviations. That is, for d
#' days we compute SD_1 ... SD_d daily standard deviations and calculate
#' \eqn{1/d * \sum [(SD_i)]}
#'
#' \item SdHHMM - between time points:
#'
#' Calculated by taking the mean glucose values at each time point in the grid across days,
#' and taking the standard deviation of those mans. That is, for t time points
#' we compute X_t means for each time point and then compute SD([X_1, X_2, ... X_t]).
#'
#' \item SdwSH - within series:
#'
#' Calculated by taking the hour-long intervals starting at every point in the interpolated
#' grid, computing the standard deviation of the points in each hour-long interval, and then
#' finding the mean of those standard deviations. That is, for n time points compute
#' SD_1 ... SD_n, where SD_i is the standard deviation of the set [SD_i, SD_i+2, ... SD_k-1]
#' where SD_k is the first measurement more than an hour later than SD_1. Then, take
#' \eqn{1/n * \sum [(SD_i)]}.
#'
#' \item SdDM - horizontal sd:
#'
#' Calcualted by taking the daily mean glucose values, and then taking the standard deviation
#' of those daily means. That is, for d days we take X_1 ... X_d daily means, and then compute
#' SD([X_1, X_2, ... X_d]).
#'
#' \item SdB -  between days, within timepoints:
#'
#' Calculated by taking the standard deviation of the glucose values across days for each time point,
#' and then taking the mean of those standard deviations.
#' That is, for t time points take SD_1 ... SD_t standard deviations, and then compute
#' \eqn{1/t * \sum[(SD_i)]}
#'
#' \item SdBDM - between days, within timepoints, corrected for changes in daily means:
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
      out = data.frame(id = NA, SdW = mean(apply(gd2d, 1, sd, na.rm = T), na.rm = T))
      # SdHHMM - between time points
      out$SdHHMM = sd(apply(gd2d, 2, mean, na.rm = T), na.rm = T)
      # SdWSH - Within series - for 1 hour window
      win = round(60/dt0)
      gs = as.vector(t(gd2d))
      N = length(gs)
      ind = rep(1:ceiling(N/win), each = win)[1:N]
      out$SdWSH = mean(tapply(gs, ind, sd, na.rm = T), na.rm = T)

      # SdDM - "Horizontal" sd
      meanR = apply(gd2d, 1, mean, na.rm = T)
      out$SdDM = sd(meanR, na.rm = T)

      # SdB - between days, within timepoints
      out$SdB = mean(apply(gd2d, 2, sd, na.rm = T), na.rm = T)

      # SdBDM - between days, within timepoints, corrected for changes in daily means
      med = matrix(rep(meanR, each = ncol(gd2d)), ncol = ncol(gd2d), byrow = T)
      # out$SdBDM = mean(apply(sqrt((gd2d - med)^2), 1, mean, na.rm = T), na.rm = T)
      out$SdBDM = mean(apply(gd2d - med, 2, sd, na.rm = T), na.rm = T)

      out
    })

  results = dplyr::bind_rows(results) %>%
    dplyr::mutate(id = unique(data$id))

  return(results)
}
