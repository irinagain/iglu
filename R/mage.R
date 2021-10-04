#' Calculate Mean Amplitude of Glycemic Excursions
#'
#' @description The function calculates MAGE values and can optionally return a plot of the glucose trace.
#'
#' @param data Data Frame object with column names "id", "time", and "gl" OR numeric vector of glucose values (plot won't work with vector).
#'
#' @param version Either \code{'ma'} or \code{'naive'}. Chooses which version of the MAGE algorithm to use. \code{'ma'} algorithm is more accurate, and is the default. Earlier versions of iglu package (<=2.0.0) used \code{'naive'}.
#'
#' @param sd_multiplier A numeric value that can change the sd value used to determine size of glycemic excursions used in the calculation. This parameter is only used when \code{version = "naive"}, ans is ignored otherwise.
#'
#' @param ... Optional arguments to pass to the MAGE Function when \code{'ma'} algorithm is selected (see \code{\link{mage_ma_single}})
#' \itemize{
#'   \item{dateformat: POSIXct format for time of glucose reading. Default: YYYY-mm-dd HH:MM:SS. Highly recommended to set if glucose times are of a different format.}
#'   \item{short_ma: Integer for period length of the short moving average. Must be positive and less than "long_MA". Default: 5. (Recommended <15).}
#'   \item{long_ma: Integer for period length for the long moving average. Default: 23. (Recommended >20)}
#'  \item{plot: Boolean. Returns ggplot if TRUE. Default: FALSE.}
#'  \item{interval: Integer for time interval in minutes between glucose readings. Algorithm will auto-magically determine the interval if not specified. Default: NA (Only used to calculate the gaps shown on the ggplot)}
#'  \item{title: Title for the ggplot. Default: "Glucose Trace - Subject [ID]"}
#'  \item{xlab: Label for x-axis of ggplot. Defaults to "Time"}
#'  \item{ylab: Label for y-axis of ggplot. Defaults to "Glucose Level"}
#'  \item{show_ma: Whether to show the moving average lines on the plot or not. Default: FALSE}
#' }
#'
#' @return A tibble object with two columns: the subject id and corresponding MAGE value. If a vector of glucose values is passed, then a tibble object with just the MAGE value is returned. In version 2, if \code{plot = TRUE}, a "master" ggplot will be returned with glucose traces for all subjects.
#' @export
#'
#' @details The function computationally emulates the manual method for calculating the mean amplitude of glycemic excursions (MAGE) first suggested in Mean Amplitude of Glycemic Excursions, a Measure of Diabetic Instability, (Service, 1970).
#'
#' \code{'ma'} is a more accurate algorithm that uses the crosses of a short and long moving average to identify intervals where a peak/nadir might exist. Then, the height from one peak/nadir to the next nadir/peak is calculated from the *original* glucose values.
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



mage <- function(data, version = c('ma', 'naive'), sd_multiplier = 1, ...) {

  # Match version
  version = match.arg(version)

  if(version == 'naive') {
    warning("You use the naive version of the iglu mage algorithm. It is included for backward compatibility with earlier versions of iglu and is less accurate than the ma algorithm.")
    return(mage_sd(data, sd_multiplier = sd_multiplier))
  }

  return(mage_ma(data, ...))
}

mage_ma <- function(data, ...) {
  id = . = MAGE = NULL
  rm(list = c("id", ".", "MAGE"))

  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out <- data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::do(MAGE = mage_ma_single(., ...))

  # Check if a ggplot or number in list is returned - convert the latter to a number
  if(is.numeric(out$MAGE[[1]])|is.na(out$MAGE[[1]])) {
    out <- out %>% dplyr::mutate(MAGE = as.numeric(MAGE))
  }

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
    dplyr::summarise(
      MAGE = mean(
        abs_diff_mean[abs_diff_mean > (sd_multiplier * sd(gl, na.rm = TRUE))],
        na.rm = TRUE)
    )
  #if (is_vector) {
  #  out$id = NULL
  #}
  return(out)

}

