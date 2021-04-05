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
#' By default, a tibble object with 1 row for each subject, and 13 columns is returned:
#' a column for subject id,
#' a column for start date,
#' a column for end date,
#' a column for number of days,
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
#' The function uses recommended cutoffs of 54, 70, 180, and 250 mg/dL for calculation.
#'
#' If shinyformat = FALSE (default), returns a tibble object with 1 row for each subject, and 12 columns:
#' a column for subject id,
#' a column for start date,
#' a column for end date,
#' a column for number of days,
#' a column for active_percent,
#' a column for Mean value,
#' a column for gmi value,
#' a column for cv value,
#' a column for below_54 value,
#' a column for below_70 value,
#' a column for in_range_70_180 value,
#' a column for above_180 value,
#' a column for above_250 value.
#' If shinyformat = TRUE, a tibble with 2 columns: metric and value, is returned.
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

  id = . = start_date = end_date = ndays = cv = NULL
  rm(list = c("id", ".", "start_date", "end_date", "ndays", "cv"))

  activity <- active_percent(data)

  out = list("Start Date" = activity[, c(1,4)],
             "End Date" = activity[, c(1,5)],
             "Duration" = activity[, c(1,3)],
             "% Time CGM is Active" = activity[, c(1,2)],
             "Average Glucose" = mean_glu(data),
             "GMI" = gmi(data),
             "CV" = cv_glu(data),
             "Percent_Below" = below_percent(data, targets_below = c(54, 70)),
             "Percent_In_Range" = in_range_percent(data, target_ranges = list(c(70, 180))),
             "Percent_Above" = above_percent(data, targets_above = c(180, 250)))
  outTable <- out %>%
    Reduce(function(dtf1, dtf2) dplyr::left_join(dtf1, dtf2, by = "id"), .)

  if (shinyformat) {
    outTable <- outTable %>%
      dplyr::transmute("Subject ID" = as.character(id),
                "Start Date" = as.character(as.Date(start_date)),
                "End Date" = as.character(as.Date(end_date)),
                "Duration" = paste(as.character(ndays),"days"),
                "% Time CGM is Active" = paste0(round(active_percent, 1), "%"),
                "Average Glucose" = paste(round(mean), "mg/dL"),
                "GMI" = paste0(round(GMI, 1), "%"),
                "CV" = paste0(round(CV, 1), "%")) %>%
      tidyr::pivot_longer(cols = "Subject ID":"CV", names_to = "metric", values_to = "value")
  }

  return(outTable)
}

