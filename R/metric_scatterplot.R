#' Plot scatterplot of metric comparisons
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' @param metric1
#' @param metric2
#'
#' @return
#'
#' @export
metric_scatter <- function(
  data,
  metric1 = c('adrr',
              'grade_hypo', 'hypo_index',
              'grade_hyper', 'hyper_index',
              'grade_eugly', 'igc',
              "mage", "modd"),
  metric2 = 'conga',
  subjects = NULL,
  lower = 70,
  upper = 140,
  targets_above = c(140, 180, 200, 250),
  targets_below = c(50,80),
  target_ranges = list(c(80,200), c(70,180), c(70,140)),
  quantiles = c(0, 25, 50, 75, 100),
  sd_multiplier = 1,
  lag = 1
){
  metric1 = match.arg(metric1)
  if(metric1 %in% c('above_percent', 'below_percent', 'in_range_percent',
                    'summary_glu', 'quantile_glu', 'plot_glu')){
    stop('metric_scatter currently only supports metrics with a single column
         in their output.')
  }

  string1 = make_metric_string(metric1, lower, upper, targets_above,
                               targets_below, target_ranges, quantiles,
                               sd_multiplier, lag)
  string2 = make_metric_string(metric2, lower, upper, targets_above,
                               targets_below, target_ranges, quantiles,
                               sd_multiplier, lag)



}

make_metric_string <- function(
  metric, lower, upper, targets_above,
  targets_below, target_ranges, quantiles,
  sd_multiplier, lag){
  # Lower only
  if (metric %in% c('grade_hypo', 'hypo_index')){
    string = paste('iglu::', metric, '(data, lower = ', lower, ')', sep = '')
  }
  # Upper only
  else if (metric %in% c('grade_hyper', 'hyper_index')) {
    string = paste('iglu::', metric, '(data, upper = ', upper, ')', sep = '')
  }
  # Lower and Upper
  else if (metric %in% c('grade_eugly', 'igc')){
    string = paste('iglu::', metric, '(data, lower = ', lower, ', upper = ', upper, ')', sep = '')
  }
  # sd_multiplier
  else if (metric %in% c('mage')){
    string = paste('iglu::', metric, '(data, sd_multiplier = ', sd_multiplier, ')', sep = '')
  }
  # lag
  else if (metric %in% c('modd')){
    string = paste('iglu::', metric, '(data, lag = ', lag, ')', sep = '')
  }
  # No params
  else {
    string = paste('iglu::', metric, '(data)', sep = '')
  }
  return(string)
}
