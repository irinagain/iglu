#' Plot Ambulatory Glucose Profile (AGP) modal day
#'
#' @description
#' The function plot_agp produces an AGP plot that collapses all data into a single 24 hr "modal day".
#'
#' @usage
#' plot_agp(data, LLTR = 70, ULTR = 180, dt0 = NULL, inter_gap = 45, tz = "")
#'
#' @inheritParams CGMS2DayByDay
#' @inheritParams plot_glu
#'
#' @return Plot of a 24 hr modal day collapsing all data to a single day.
#'
#' @export
#'
#' @author Elizabeth Chun
#'
#' @details
#' Only a single subject's data may be plotted. The black line is the median glucose value for each time of day. The dark blue shaded area
#' represents 50\% of glucose values - those between the 25th and 75 quartiles. The light
#' blue shaded area shows 90\% of the glucose values - those between the 5th and 95th quartiles.
#' The horizontal green lines represent the target range, default is 70-180 mg/dL. Additionally,
#' the percents shown on the right hand side of the plot show which quartile each line refers to -
#' e.g. the line ending at 95\% is the line corresponding to the 95th quartile of glucose values.
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
#' plot_agp(example_data_1_subject)
#'

plot_agp <- function (data, LLTR = 70, ULTR = 180, dt0 = NULL, inter_gap = 45, tz = "") {

  gl = id = five = twentyfive = seventyfive = ninetyfive = NULL
  rm(list = c("gl",  "id", "five", "twentyfive", "seventyfive", "ninetyfive"))

  subject = unique(data$id)
  ns = length(subject)
  if (ns > 1){
    subject = subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data %>% dplyr::filter(id == subject)
  }

  data_ip = CGMS2DayByDay(data, dt0 = dt0, inter_gap = inter_gap, tz = tz)
  gl_ip = data_ip[[1]]
  quartiles <- apply(gl_ip, 2, quantile, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm = TRUE)
  q_labels <- dplyr::as_tibble(quartiles[, ncol(quartiles)])

  plot_data = dplyr::tibble(
    times = hms::as_hms(seq(data_ip[[3]]*60, 86400, by = data_ip[[3]]*60)),
    median = quartiles[3, ], five = quartiles[1, ], twentyfive = quartiles[2, ],
    seventyfive = quartiles[4, ], ninetyfive = quartiles[5, ]
  )

  ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(times, median), color = "black", size = 1) +
    ggplot2::geom_line(ggplot2::aes(times, five), linetype = "longdash", color = "#325DAA") +
    ggplot2::geom_line(ggplot2::aes(times, ninetyfive), linetype = "longdash", color = "#325DAA") +
    ggplot2::geom_ribbon(ggplot2::aes(times, ymin = seventyfive, ymax = ninetyfive),
                         fill = "#A7BEE7", alpha = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(times, ymin = five, ymax = twentyfive),
                         fill = "#A7BEE7", alpha = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(times, ymin = twentyfive, ymax = seventyfive),
                         fill = "#325DAA", alpha = 0.5) +
    ggplot2::geom_hline(yintercept = LLTR, color = '#48BA3C') +
    ggplot2::geom_hline(yintercept = ULTR, color = '#48BA3C') +
    ggplot2::scale_x_time(breaks = c(hms::as_hms(c('00:00:00', '03:00:00', '06:00:00', '09:00:00', '12:00:00',
                                                   '15:00:00', '18:00:00', '21:00:00', '24:00:00'))),
                          labels = c('12 am', '3 am', '6 am', '9 am', '12 pm',
                                     '3 pm', '6 pm', '9 pm', '12 am')) +
    ggplot2::ylab("Glucose mg/dL") + ggplot2::xlab(NULL) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(1,3,1,1), "lines")) +
    ggplot2::geom_text(data = q_labels,
                       ggplot2::aes(label = c("5%", "25%", "50%", "75%", "95%"), y = value),
                       x = 90700, hjust = 0, size = 3.25) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(plot.margin = ggplot2::unit(c(1,3,1,1), units = "line"))

}
