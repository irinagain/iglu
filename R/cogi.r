#' Calculate Continuous Glucose Monitoring Index (COGI) values
#'
#' @description
#' The function COGI produces cogi values in a tibble object.
#'
#' @usage
#' cogi(data, targets = c(70, 180), weights = c(.5,.35,.15))
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or numeric vector of glucose values.
#'
#' @param targets Numeric vector of two glucose values for threshold. Glucose values from
#' data argument will be compared to each value in the targets vector to determine the
#' time in range and time below range for COGI. The lower value will be used for determining
#' time below range.
#' Default list is (70, 180).
#'
#' @param weights Numeric vector of three weights to be applied to time in range, time below range
#' and glucose variability, respectively. The default list is (.5,.35,.15)
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' column for each target value is returned. NA's will be omitted from the glucose
#' values in calculation of cogi.
#'
#'
#' @return If a data.frame object is passed, then a tibble object with
#' a column for subject id and then a column for each target value is returned. If a vector of glucose
#' values is passed, then a tibble object without the subject id is returned.
#' as.numeric() can be wrapped around the latter to output a numeric vector.
#'
#' @export
#'
#' @references
#' Leelarathna (2020) Evaluating Glucose Control With a Novel Composite
#' Continuous Glucose Monitoring Index,
#' \emph{Diabetes Technology and Therapeutics} \strong{14(2)} 277-284,
#' \doi{10.1177/1932296819838525}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#'
#' cogi(example_data_1_subject)
#' cogi(example_data_1_subject, targets = c(50, 140), weights = c(.3,.6,.1))
#'
#' data(example_data_5_subject)
#'
#' cogi(example_data_5_subject)
#' cogi(example_data_5_subject, targets = c(80, 180), weights = c(.2,.4,.4))
#'


cogi <- function(data, targets = c(70,180), weights = c(.5,.35,.15)){
  id = w_f = ir = br = stddev = weight_features = feature = scale_range = weight = NULL
  rm(list = c("id", "w_f", "ir", "br", "stddev", "weight_features", "feature", "scale_range", "weight"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  targets = sort(as.double(targets))

  weight_features = function(feature, scale_range, weight = 1, increasing = FALSE) {
    if (increasing) {
      out = pmin(as.numeric(feature>min(scale_range))*(feature-min(scale_range))/(max(scale_range)-min(scale_range)), 1)
      return(out*weight)
    } else {
      out = pmin(as.numeric(feature<max(scale_range))*(feature-max(scale_range))/(min(scale_range)-max(scale_range)), 1)
      return(out*weight)
    }
  }
  ir = in_range_percent(data, list(targets))[,2]
  br = below_percent(data, targets_below = targets[1])[,2]
  stddev = sd_glu(data)$SD
  weighted_features = weight_features(ir,c(0,100),weight = weights[1], increasing = TRUE)+weight_features(br,c(0,15),weight = weights[2])+weight_features(stddev,c(18,108),weight = weights[3])
  out = dplyr::tibble(weighted_features)
  out$id = sd_glu(data)$id
  out = out[,c(2,1)]
  colnames(out) = c("id", "COGI")
  out$COGI = out$COGI*100 # convert to percent

  if (is_vector) {
    out$id = NULL
  }

  return(out)
}
