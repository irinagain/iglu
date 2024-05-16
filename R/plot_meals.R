#' Plot meal metrics visualization
#'
#' @description
#' The function plot_meals produces a visual for meals data
#'
#' @usage
#' plot_meals(data, mealtimes, plot_type=c('ggplot','plotly'))
#'
#' @inheritParams meal_metrics
#' @inheritParams mage_ma_single
#'
#' @return Plot to visualize meals data.
#'
#' @export
#'
#' @author Elizabeth Chun
#'
#' @details
#' Only a single subject's data may be plotted. The solid black line is the glucose trace.
#' Vertical dashed red lines show the mealtimes, and the horizontal blue lines
#' show the baseline for each meal. Purple triangles are plotted to illustrate the 3 meal_metrics
#' Namely the three vertices show the baseline, peak, and 1hr post-peak recovery. If
#' plot_type = 'plotly', plotly is used to display an interactive visual that allows
#' one to zoom into specific areas of the plot.
#'
#' @seealso meal_metrics()
#'
#' @references
#' Service, F. John. (2013) Glucose Variability, \emph{Diabetes}
#' \strong{62(5)}: 1398-1404, \doi{10.2337/db12-1396}
#'
#'
#' @examples
#'
#' data(example_data_hall)
#' data(example_meals_hall)
#' meal_metrics(example_data_hall, example_meals_hall)
#'

plot_meals = function(data, mealtimes, plot_type=c('ggplot','plotly')) {

  id = meal = mealtime = peaktime = recovertime = basegl = peakgl = recovergl =
    time_window = x = y = NULL
  rm(list = c("id", "meal", "mealtime", "peaktime", "recovertime", "basegl", "peakgl", "recovergl",
              "time_window", "x", "y"))

  #Checking for more than 1 subject
  ns = length(unique(data$id))
  if (ns > 1){
    subject = unique(data$id)[1]
    warning(paste("The provided data have", ns, "subjects. The plot will only be created for subject", subject))
    data = data[data$id == subject, ]
  }

  metrics = meal_metrics(data, mealtimes, glucose_times = TRUE)

  metrics_expanded = metrics %>%
    dplyr::rowwise() %>%
    dplyr::reframe(id = id, meal = meal,
      time_window = (time - 1*60*60) + lubridate::seconds(0:14400),
      basegl = basegl, peakgl = peakgl, recovergl = recovergl,
      mealtime = time, peaktime = peaktime, recovertime = recovertime
    )

  # all data with requisite meal labeling for glucose and times
  merged = dplyr::left_join(data, metrics_expanded, dplyr::join_by("id" == "id", "time" == "time_window"))
  # coordinates for triangle
  coords = tibble::tibble(
    x = rep(as.POSIXct(NA), 3*nrow(metrics)),
    y = rep(0, 3*nrow(metrics)),
    meal = rep(NA_character_, 3*nrow(metrics))
  )

  meal_idx = 1
  for (i in seq(1, nrow(coords), by = 3)) {
    coords$x[i:(i+2)] = c(metrics$time[meal_idx], metrics$peaktime[meal_idx], metrics$recovertime[meal_idx])
    coords$y[i:(i+2)] = c(metrics$basegl[meal_idx], metrics$peakgl[meal_idx], metrics$recovergl[meal_idx])
    coords$meal[i:(i+2)] = rep(metrics$meal[meal_idx], 3)
    meal_idx = meal_idx + 1
  }

  meals_plot = ggplot2::ggplot(merged) +
    # gl time-series
    ggplot2::geom_line(ggplot2::aes(time, gl)) +
    # baseline glucose
    ggplot2::geom_line(ggplot2::aes(time, basegl, group = meal), color = "dodgerblue") +
    # mealtime vertical line
    ggplot2::geom_vline(xintercept = as.numeric(unique(merged$mealtime[!is.na(merged$mealtime)])),
               color = "red", linetype = "dashed") +
    ggplot2::geom_polygon(data = coords, ggplot2::aes(x, y, group = meal), fill = "mediumpurple1", alpha = 0.5) +
    ggplot2::ggtitle(label = "Meal Metrics Plot") +
    ggplot2::xlab("Time") + ggplot2::ylab("Glucose (mg/dl)")

  if (plot_type == 'plotly') {
    out =  plotly::ggplotly(meals_plot)
  } else {
    out = meals_plot
  }

  out
}
