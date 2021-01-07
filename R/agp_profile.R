
#' Display Ambulatory Glucose Profile (AGP) statistics for selected subject
#'
#' @usage
#' agp(data, inter_gap = 45, dt0 = NULL, tz = "", daily = TRUE)
#'
#' @inheritParams CGMS2DayByDay
#' @param daily Logical indicator whether AGP should include separate daily plots. The default value is TRUE
#'
#' @return A plot displaying glucose measurements range, selected glucose statistics (average glucose, Glucose Management Indicator, %CV), percentage spent in target ranges and quantiles of 24 hour profile.
#'
#' @export
#'
#'@references
#' Johnson et al. (2019) Utilizing the Ambulatory Glucose Profile to Standardize and
#' Implement Continuous Glucose Monitoring in Clinical Practice,
#' \emph{Diabetes Technology and Therapeutics} \strong{21:S2} S2-17-S2-25,
#' \doi{10.1089/dia.2019.0034}.
#'
#' @examples
#' data(example_data_1_subject)
#' agp(example_data_1_subject)
#' agp(example_data_1_subject, daily = FALSE)
agp <- function(data, inter_gap = 45, dt0 = NULL, tz = "", daily = TRUE){

  id = NULL
  rm(list = c("id"))

  subject = unique(data$id)
  ns = length(subject)
  if (ns > 1){
    subject = subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data %>% dplyr::filter(id == subject)
  }

  # Calculate range of measurements
  out_range = active_percent(data)

  # Calculate metrics
  tableStat = data.frame("Glucose statistics", "Value")
  tableStat[1, 1] = "ID"
  tableStat[1, 2] = as.character(subject)
  tableStat[2, 1] = "Start Date"
  tableStat[2, 2] = as.character(out_range$start_date)
  tableStat[3, 1] = "End Date"
  tableStat[3, 2] = as.character(out_range$end_date)
  tableStat[4, 1] = "Duration"
  tableStat[4, 2] = paste(as.character(out_range$ndays),"days")
  tableStat[5, 1] = "% Time CGM is Active"
  tableStat[5, 2] = paste0(round(out_range$active_percent, 1), "%")
  tableStat[6, 1] = "Average Glucose"
  tableStat[6, 2] = paste(round(mean_glu(data)$mean), "mg/dL")
  tableStat[7, 1] = "Glucose Management Indicator (GMI)"
  tableStat[7, 2] = paste0(round(gmi(data)$gmi, 1), "%")
  tableStat[8, 1] = "Glucose Variability (CV)"
  tableStat[8, 2] = paste0(round(cv_glu(data)$cv, 1), "%")

  # Make a pretty table
  mytheme <- gridExtra::ttheme_default(core = list(fg_params = list(hjust = 0, x = 0.03, fontsize = 12)), colhead = list(fg_params = list(fontsize=9, fontface="bold")))

  t1 = gridExtra::tableGrob(tableStat, rows = NULL, cols = NULL, theme = mytheme)

  # Create percentage plot
  p1 = plot_ranges(data)

  # Create AGP plot
  p2 = plot_agp(data, dt0 = dt0, inter_gap = inter_gap, tz = tz)

  if (daily){
    # Create daily plots
    p3 = plot_daily(data, inter_gap = inter_gap, tz = tz)

    # Combine metrics and plot in one display
    pFinal = (wrap_elements(t1) +  p1 + guide_area() + plot_layout(widths = c(3, 1, 1))) / p2 / p3
  }else{
    pFinal = (wrap_elements(t1) +  p1 + guide_area() + plot_layout(widths = c(3, 1, 1))) / p2
  }
  pFinal
}


