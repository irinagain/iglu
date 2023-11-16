#' Calculate all metrics in iglu
#'
#' @description
#' The function all_metrics runs all of the iglu metrics, and returns the results with
#' one column per metric.
#'
#' @usage
#' all_metrics(data, dt0 = NULL, inter_gap = 45, tz = "", timelag = 15, lag = 1)
#'
#' @param data DataFrame object with column names "id", "time", and "gl".
#' @inheritParams optimized_iglu_functions
#'
#' @return
#' A tibble object with 1 row per subject and one column per metric is returned.
#'
#' @export
#'
#' @details
#' All iglu functions are calculated within the all_metrics function, and the resulting tibble
#' is returned with one row per subject and a column for each metric. Time dependent functions are
#' calculated together using the function optimized_iglu_functions with two exceptions:
#' PGS and episodes are calculated within all_metrics because their structure does not
#' align with optimized_iglu_functions. Note that episodes related outputs included in all_metrics are only
#' average episodes per day. To get the average duration and glucose, please use the standalone
#' episodes function
#'
#' For metric specific information, please see the corresponding function documentation.
#'
#' @examples
#' data(example_data_1_subject)
#' all_metrics(example_data_1_subject)
#'
#'
#' # Specify the meter frequency and change the interpolation gap to 30 min
#' all_metrics(example_data_1_subject, dt0 = 5, inter_gap = 30)
#'


# function calls all metrics on a dataset.
# returns a list
all_metrics <- function(data, dt0 = NULL, inter_gap = 45, tz = "", timelag = 15, lag = 1){
  . = id = type = level = avg_ep_per_day = lvl = NULL
  rm(list = c(".", 'id', 'type', 'level', 'avg_ep_per_day', 'lvl'))

  # reformat episodes to give one row per subject and one column per category for avg_ep_per_day
  episodes = episode_calculation(data)
  ep_out = episodes %>%
    dplyr::select(id, type, level, avg_ep_per_day) %>%
    dplyr::mutate(
      lvl = paste0(type, "_", level)
    ) %>%
    dplyr::select(id, lvl, avg_ep_per_day) %>%
    dplyr::group_by(id) %>%
    tidyr::pivot_wider(
      names_from = 'lvl',
      values_from = 'avg_ep_per_day'
    )

  # Mean, Median, and Quantile Metrics not included. Summary covers all
  out = list("ADRR" = adrr(data),
             "COGI" = cogi(data),
             "CV_GLU" = cv_glu(data),
             "eA1C" = ea1c(data),
             "episodes" = ep_out,
             "GMI" = gmi(data),
             "GRI" = gri(data),
             "GRADE" = grade(data),
             "GRADE_Euglycemia" = grade_eugly(data),
             "GRADE_Hyperglycemia" = grade_hyper(data),
             "GRADE_Hypoglycemia" = grade_hypo(data),
             "HBGI" = hbgi(data),
             "LBGI" = lbgi(data),
             "Hyper_Index" = hyper_index(data),
             "Hypo_Index" = hypo_index(data),
             "IGC" = igc(data),
             "IQR_GLU" = iqr_glu(data),
             "J_Index" = j_index(data),
             "M_Value" = m_value(data),
             "Mad_GLU" = mad_glu(data),
             "MAGE" = mage(data),
             "Percent_Active" = active_percent(data),
             "Percent_Above" = above_percent(data),
             "Percent_Below" = below_percent(data),
             "Percent_In_Range" = in_range_percent(data),
             "PGS" = pgs(data),
             "Range" = range_glu(data),
             "SD_GLU" = sd_glu(data),
             "Summary" = summary_glu(data),
             optimized_iglu_functions(data, dt0, inter_gap, tz, timelag, lag))
  outTable <- out %>%
    Reduce(function(dtf1,dtf2) dplyr::left_join(dtf1, dtf2, by = "id"), .)
  return(outTable)
}

