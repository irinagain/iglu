#' Calculate the standard deviation of the rate of change
#'
#' @description
#' The function sd_roc produces the standard deviation of the rate of change
#' values in a tibble object.
#'
#' @usage
#' sd_roc(data, timelag = 15, dt0 = NULL, inter_gap = 45, tz = "")
#'
#' @inheritParams roc
#'
#' @return A tibble object with two columns: subject id and standard deviation
#' of the rate of change values for each subject.
#'
#' @export
#'
#' @details
#' A tibble object with one row for each subject, a column for subject id
#' and a column for the standard deviation of the rate of change.
#'
#' When calculating rate of change, missing values will be linearly interpolated
#' when close enough to non-missing values.
#'
#' Calculated by taking the standard deviation of all the ROC values for each
#' individual subject. NA rate of change values are omitted from the
#' standard deviation calculation.
#'
#' @author Elizabeth Chun, David Buchanan
#'
#' @references
#' Clarke et al. (2009) Statistical Tools to Analyze Continuous Glucose Monitor Data,
#' Diabetes
#' \emph{Diabetes Technology and Therapeutics} \strong{11} S45-S54,
#' \doi{10.1089/dia.2008.0138}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' sd_roc(example_data_1_subject)
#' sd_roc(example_data_1_subject, timelag = 10)
#'
#' data(example_data_5_subject)
#' sd_roc(example_data_5_subject)
#' sd_roc(example_data_5_subject, timelag = 10)
#'

sd_roc <- function(data, timelag = 15, dt0 = NULL, inter_gap = 45, tz = ""){
  gl = id = sd_roc = NULL
  rm(list = c("gl", "id", "sd_roc"))
  data = check_data_columns(data)

  out = roc(data, timelag, dt0, inter_gap, tz) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      sd_roc = sd(roc, na.rm = TRUE)
    )
  return(out)
}
