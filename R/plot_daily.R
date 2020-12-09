#' Plot daily glucose profiles
#'
#' @description
#' The function plot_daily plots daily glucose time series profiles for a single subject.
#'
#' @usage
#' plot_daily(data, LLTR = 70, ULTR = 180, inter_gap = 45, tz = "")
#'
#' @inheritParams plot_glu
#'
#' @return Daily glucose time series plots for a single subject
#'
#' @export
#'
#' @author Elizabeth Chun
#'
#' @details
#' Only a single subject's data may be plotted. The black line shows the glucose values.
#' The shaded gray area shows the target range, default 70-180 mg/dL. Areas of the curve
#' above the ULTR are shaded yellow, while areas below the LLTR are shaded red.
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
#' plot_daily(example_data_1_subject)
#' plot_daily(example_data_1_subject, LLTR = 100, ULTR = 140)
#'

plot_daily <- function (data, LLTR = 70, ULTR = 180, inter_gap = 45, tz = "") {

  gl =  id = NULL
  rm(list = c("gl", "id"))
  if (!lubridate::is.POSIXct(data$time)){ # Check if already in date format
    data$time = as.character(data$time)
    data$time = as.POSIXct(data$time, format='%Y-%m-%d %H:%M:%S', tz = tz)
    # Check if any NAs from conversion, this happens if wrong time format (e.g. 25:00:00) or wrong time zone which will affect daylight savings time
    if (any(is.na(data$time))){
      warning(paste("During time conversion,", sum(is.na(data$time)), "values were set to NA. Check the correct time zone specification."))
    }
  }

  subject = unique(data$id)
  ns = length(subject)
  if (ns > 1){
    subject = subject[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data %>% dplyr::filter(id == subject)
  }

  plot_data <- data %>%
    dplyr::mutate(day_of_week = as.character(lubridate::wday(time, label = TRUE, abbr = FALSE)),
                  each_day = lubridate::date(time),
                  reltime = hms::as_hms(paste(lubridate::hour(time), lubridate::minute(time), lubridate::second(time), sep = ":")),
                  gl_level = dplyr::case_when(gl > ULTR ~ "hyper", gl < LLTR ~ "hypo", TRUE ~ "normal"))

  gl_level <- plot_data %>%
    dplyr::mutate(level_group = rep(1:length(rle(gl_level)[[1]]), rle(gl_level)[[1]])) %>%
    dplyr::group_by(level_group) %>%
    dplyr::summarise(id = id[1], time = c(time[1] - 10, time, time[dplyr::n()] + 10),
                     reltime = hms::as_hms(c(reltime[1] - 10, reltime, reltime[dplyr::n()] + 10)),
                     gl = dplyr::case_when(
                       gl_level[1] == "hyper" ~ c(ULTR, gl, ULTR),
                       gl_level[1] == "hypo" ~  c(LLTR, gl, LLTR)),
                     day_of_week = c(day_of_week[1], day_of_week, day_of_week[dplyr::n()]),
                     each_day = c(each_day[1], each_day, each_day[dplyr::n()]),
                     class = gl_level[1], .groups = "drop")
  if (!any(gl_level$class == "hypo")) { # if no hypo/hyper, add row for geom_ribbon
    gl_level = dplyr::add_row(gl_level, gl_level[1, ])
    gl_level$class[1] <- "hypo"
    gl_level$gl[1] <- LLTR
  }
  if (!any(gl_level$class == "hyper")) {
    gl_level = dplyr::add_row(gl_level, gl_level[1, ])
    gl_level$class[1] <- "hyper"
    gl_level$gl[1] <- ULTR
  }

  plot_data <- plot_data[complete.cases(plot_data), ] %>%
    dplyr::group_by(id) %>%
    dplyr::arrange(data.frame(id, time, gl), time) %>%
    dplyr::ungroup() # ensure ascending time by subject
  gaps <- plot_data %>%
    dplyr::mutate(gap = ifelse(difftime(time, dplyr::lag(time), units = "mins") > inter_gap,
                               TRUE, FALSE), row = 1:length(time)) %>%
    dplyr::slice(1, which(gap))
  gaps <- c(gaps$row, nrow(plot_data) + 1)
  plot_data <- plot_data %>%
    dplyr::mutate(time_group = rep(1:(length(gaps) - 1), diff(gaps))) # group by consecutive times to avoid artifacts

  ggplot2::ggplot(plot_data) +
    ggplot2::geom_line(ggplot2::aes(reltime, gl, group = time_group)) +
    ggplot2::geom_ribbon(ggplot2::aes(reltime, ymin = LLTR, ymax = ULTR),
                         fill = "lightgrey", alpha = 0.5) +
    ggplot2::geom_ribbon(data = gl_level[gl_level$class == "hyper", ],
                         ggplot2::aes(reltime, ymin = ULTR, ymax = gl),
                         fill = "yellow", alpha = 0.5) +
    ggplot2::geom_ribbon(data = gl_level[gl_level$class == "hypo", ],
                         ggplot2::aes(reltime, ymin = LLTR, ymax = gl),
                         fill = "red", alpha = 0.5) +
    ggplot2::scale_x_time(breaks = c(hms::as_hms(c('00:00:00', '12:00:00', '24:00:00'))),
                          labels = c('12 am', '12 pm', '12 am')) +
    ggplot2::facet_wrap(~each_day + day_of_week, ncol = 7, ) +
    ggplot2::ylab("Glucose mg/dL") + ggplot2::xlab(NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
                   panel.background = ggplot2::element_rect(fill = "transparent", colour = NA))

}
