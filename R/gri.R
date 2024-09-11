#' Calculate Glycemia Risk Index (GRI)
#'
#' @description
#' The function gri produces a tibble object with values equal to
#' the glycemia risk index (GRI) metric. The output columns are subject id and
#' GRI value. ' The output rows correspond to subjects.
#'
#' @usage
#' gri(data)
#'
#' @inheritParams mean_glu
#'
#' @param tz \strong{Default: "".} A character string specifying the time zone to be used. System-specific (see \code{\link{as.POSIXct}}), but " " is the current time zone, and "GMT" is UTC (Universal Time, Coordinated). Invalid values are most commonly treated as UTC, on some platforms with a warning.
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' column for GRI is returned. The formula for GRI is as follows:
#'
#' \eqn{(3.0 × VLow) + (2.4 × Low) + (1.6 × VHigh) + (0.8 × High)}
#'
#' where VLow, Low, VHigh, and High correspond to the percent of glucose values
#' in the ranges <54 mg/dL, 54-70 mg/dL, >250 mg/dL, and 180-250 mg/dL respectively.
#' The maximum allowed value for GRI is 100\%, any calculated values higher than
#' 100 are capped.
#'
#' @return A tibble object with columns for subject id and GRI value. Rows
#' correspond to individual subjects.
#'
#' @author Elizabeth Chun
#'
#' @export
#'
#' @references
#' Klonoff et al. (2022) A Glycemia Risk Index (GRI) of Hypoglycemia and Hyperglycemia
#' for Continuous Glucose Monitoring Validated by Clinician Ratings.
#' \emph{J Diabetes Sci Technol}
#' \doi{10.1177/19322968221085273}.

#' @examples
#'
#' data(example_data_1_subject)
#' gri(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' gri(example_data_5_subject, tz = 'GMT')
#'

gri <- function(data, tz = ""){

  gri_single <- function(data) {
    # get percent in each range from agp aggregated metrics
    range_percents <- agp_metrics(data)
    range_percents <- range_percents[c("below_54", "below_70", "above_180", "above_250")]

    out = 3*range_percents$below_54 + 2.4*range_percents$below_70 +
      1.6*range_percents$above_250 + 0.8*range_percents$above_180

    # threshold at 100%
    out = ifelse(out > 100, 100, out)

    return(out)
  }

  id = NULL
  rm(list = c("id"))
  data = check_data_columns(data, time_check = TRUE, tz = tz)

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(GRI = gri_single(data.frame(id,time,gl)))

  return(out)
}


