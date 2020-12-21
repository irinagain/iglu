#' Plot Time in Ranges as a bar plot
#'
#' @description
#' The function plot_ranges produces a barplot showing the percent of time in glucose ranges.
#'
#' @usage
#' plot_ranges(data)
#'
#' @inheritParams CGMS2DayByDay
#'
#' @return Single subject bar chart showing percent in different glucose ranges.
#'
#' @export
#'
#' @author Elizabeth Chun
#'
#' @details
#' Only a single subject's data may be used. There are four ranges: very low (below 54 mg/dL),
#' low (54-69 mg/dL), target range (70-180 mg/dL), high (181-250 mg/dL), and very high (above 250 mg/dL).
#' This plot is meant to be used as part of the Ambulatory Glucose Profile (AGP)
#'
#' @references
#' Johnson et al. (2019) Utilizing the Ambulatory Glucose Profile to Standardize and
#' Implement Continuous Glucose Monitoring in Clinical Practice,
#' \emph{Diabetes Technology and Therapeutics} \strong{21:S2} S2-17-S2-25,
#' \doi{10.1089/dia.2019.0034}.
#'
#'
#' @examples
#'
#' data(example_data_1_subject)
#' plot_ranges(example_data_1_subject)
#'

plot_ranges <- function (data) {
  gl = id = NULL
  rm(list = c("gl", "id"))

  subject = unique(data$id)
  ns = length(subject)
  if (ns > 1){
    subject = subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data %>% dplyr::filter(id == subject)
  }

  ranges <- agp_metrics(data, shiny = FALSE) %>%
    dplyr::select(-c("id", "active_percent", "mean", "gmi", "cv")) %>%
    dplyr::summarise(range = c("very_low", 'low', 'target', 'high', 'very_high'),
                     percent = c(below_54, below_70 - below_54, in_range_70_180,
                                 above_180 - above_250, above_250))

  plot_data <- dplyr::tibble(range = rep(ranges$range, round(ranges$percent*100)), count = 1) %>%
    dplyr::mutate(range = factor(range, levels = c("very_high", 'high', 'target', 'low', 'very_low')))

  colors <- c("#F9B500", "#F9F000", "#48BA3C", "#F92D00", "#8E1B1B")
  ggplot2::ggplot(plot_data, ggplot2::aes(fill = range, x = count)) +
    ggplot2::geom_bar(width = 0.1, position = "fill") +
    ggplot2::coord_fixed(ratio = 0.75) +
    ggplot2::scale_fill_manual(values = colors, drop = FALSE,
                               labels = c("Very High (>250 mg/dL)", "High (181-250 mg/dL)",
                                          "Target Range (70-180 mg/dL)", "Low (54-69 mg/dL)", "Very Low (<54 mg/dL)")) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(), panel.background = ggplot2::element_blank())
}
