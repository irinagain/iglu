#' Calculate metrics for the Ambulatory Glucose Profile (AGP)
#'
#' @description
#' The function agp_metrics runs the following functions
#' and combines them into a tibble object: active_percent, mean_glu, gmi,
#' cv_glu, below_percent, in_range_percent, above_percent.
#'
#' @usage
#' agp_metrics(data, shinyformat = FALSE)
#'
#' @param data DataFrame object with column names "id", "time", and "gl".
#'
#' @param shinyformat Logical indicating whether the output should be formatted for the single subject
#' AGP page in shiny. Defaults to FALSE.
#'
#' @return
#' By default, a tibble object with 1 row for each subject, and 10 columns is returned:
#' a column for subject id,
#' a column for active_percent,
#' a column for Mean value,
#' a column for gmi value,
#' a column for cv value,
#' a column for below_54 value,
#' a column for below_70 value,
#' a column for in_range_70_180 value,
#' a column for above_180 value,
#' a column for above_250 value,
#'
#' @export
#'
#' @details
#' If shinyformat = FALSE (default), returns a tibble object with 1 row for each subject, and 12 columns:
#' a column for subject id,
#' a column for active_percent,
#' a column for Mean value,
#' a column for gmi value,
#' a column for cv value,
#' a column for below_54 value,
#' a column for below_70 value,
#' a column for in_range_70_180 value,
#' a column for above_180 value,
#' a column for above_250 value.
#' If shinyformat = TRUE, a tibble with 3 columns: id, metric, and value, is returned.
#' This output is used when generating the single subject AGP shiny page.
#'
#'
#' @references
#' Johnson et al. (2019) Utilizing the Ambulatory Glucose Profile to Standardize and
#' Implement Continuous Glucose Monitoring in Clinical Practice,
#' \emph{Diabetes Technology and Therapeutics} \strong{21:S2} S2-17-S2-25,
#' \doi{10.1089/dia.2019.0034}.
#'
#'
#' @examples
#' data(example_data_1_subject)
#' agp_metrics(example_data_1_subject)
#'


agp_metrics <- function (data, shinyformat = FALSE) {
  out = list("Percent_Active" = active_percent(data),
             "Mean_Glu" = mean_glu(data),
             "GMI" = gmi(data),
             "CV" = cv_glu(data),
             "Percent_Below" = below_percent(data, targets_below = c(54, 70)),
             "Percent_In_Range" = in_range_percent(data, target_ranges = list(c(70, 180))),
             "Percent_Above" = above_percent(data, targets_above = c(180, 250)))
  outTable <- out %>%
    Reduce(function(dtf1, dtf2) dplyr::left_join(dtf1, dtf2, by = "id"), .)

  if (shinyformat) {
    outTable <- outTable %>%
      tidyr::pivot_longer(cols = -id, names_to = "metric", values_to = "value") %>%
      dplyr::slice(c(1:4))
  }

  return(outTable)
}

