#' Calculate Mean Amplitude of Glycemic Excursions
#'
#' @description The function calculates MAGE values and can optionally return a plot of the glucose trace.
#'
#' @param data DataFrame object with column names "id", "time", and "gl" OR numeric vector of glucose values.
#'
#' @param version Either \code{'ma'} or \code{'naive'}. Chooses which version of the MAGE algorithm to use. \code{'ma'} algorithm is more accurate, and is the default. Earlier versions of iglu package (<=2.0.0) used \code{'naive'}.
#'
#' @param sd_multiplier A numeric value that can change the sd value used to determine size of
#' glycemic excursions used in the calculation. This is the only parameter that
#' can be specified for \code{version = "naive"}, and it is ignored if \code{version = "ma"}.
#'
#' @inheritParams mage_ma_single
#'
#' @return A tibble object with two columns: the subject id and corresponding MAGE value.
#' If a vector of glucose values is passed, then a tibble object with just the MAGE value
#' is returned. In \code{version = "ma"}, if \code{plot = TRUE}, a list of ggplots will
#' be returned with one plot per subject.
#' @export
#'
#' @details If version \code{'ma'} is selected, the function computationally emulates the manual method for calculating the mean amplitude of glycemic excursions (MAGE) first suggested in "Mean Amplitude of Glycemic Excursions, a Measure of Diabetic Instability", (Service, 1970). For this version, glucose values will be interpolated over a uniform time grid prior to calculation.
#'
#' \code{'ma'} is a more accurate algorithm that uses the crosses of a short and long moving average to identify intervals where a peak/nadir might exist. Then, the height from one peak/nadir to the next nadir/peak is calculated from the *original* (not moving average) glucose values.
#'
#' \code{'naive'} algorithm calculates MAGE by taking the mean of absolute glucose differences (between each value and the mean) that are greater than the standard deviation. A multiplier can be added to the standard deviation using the \code{sd_multiplier} argument.
#'
#' @references
#' Service et al. (1970) Mean amplitude of glycemic excursions, a measure of diabetic instability
#' \emph{Diabetes}  \strong{19} .644-655,
#' \doi{10.2337/diab.19.9.644}.
#'
#' @examples
#' data(example_data_5_subject)
#' mage(example_data_5_subject, version = 'ma')

mage <- function(data,
                 version = c('ma', 'naive'),
                 sd_multiplier = 1,
                 short_ma = 5, long_ma = 32,
                 return_type = c('num', 'df'),
                 return_ = c('auto', 'plus', 'minus'), # TODO: change variable name
                 plot = FALSE, dt0 = NULL, inter_gap = 45, tz = "",
                 title = NA, xlab = NA, ylab = NA, show_ma = FALSE) {

  # Match version
  version = match.arg(version)

  if(version == 'naive') {
    warning("You are using the naive version of the iglu mage algorithm. It is included for backward compatibility with earlier versions of iglu and is less accurate than the ma algorithm.")
    return(mage_sd(data, sd_multiplier = sd_multiplier))
  }

  return(mage_ma(data, short_ma = short_ma, long_ma = long_ma, return_type=return_type, return_=return_,
                 plot = plot, dt0 = dt0, inter_gap = inter_gap, tz = tz,
                 title = title, xlab = xlab, ylab = ylab, show_ma = show_ma))
}

mage_ma <- function(data,
                    short_ma = 5, long_ma = 32,
                    return_type = c('num', 'df'),
                    return_ = c('auto', 'plus', 'minus'),
                    plot = FALSE, dt0 = NULL, inter_gap = 45, tz = "",
                    title = NA, xlab = NA, ylab = NA, show_ma = FALSE) {
  id = . = MAGE = NULL
  rm(list = c("id", ".", "MAGE"))

  data = check_data_columns(data)
  is_vector = attr(data, "is_vector") # TODO: is this check really necessary? when does this return true?

  out <- data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::do(MAGE = mage_ma_single(., short_ma = short_ma, long_ma = long_ma, return_type=return_type, return_=return_,
                                    plot = plot, dt0 = dt0, inter_gap = inter_gap, tz = tz,
                                    title = title, xlab = xlab, ylab = ylab, show_ma = show_ma))

  # Check if a ggplot or number in list is returned - convert the latter to a number
  if(class(out$MAGE[[1]])[1] == "numeric" | is.na(out$MAGE[[1]][1])) {
    out <- out %>% dplyr::mutate(MAGE = as.numeric(MAGE))
  }
  # else must be ggplot output
  else {
    out <- ggpubr::ggarrange(plotlist = out$MAGE, nrow = 1, ncol = 1)
  }

  if (is_vector) {
    out$id = NULL
  }

  return(out)
}

mage_sd <- function(data, sd_multiplier = 1){

  abs_diff_mean = gl = id = NULL
  rm(list = c("gl", "id", "abs_diff_mean"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(abs_diff_mean = abs(gl - mean(gl, na.rm = TRUE))) %>%
    dplyr::summarise( # TODO: this has been deprecated - use reframe instead
      MAGE = mean(
        abs_diff_mean[abs_diff_mean > (sd_multiplier * sd(gl, na.rm = TRUE))],
        na.rm = TRUE)
    )
  if (is_vector) {
   out$id = NULL
  }
  return(out)

}

