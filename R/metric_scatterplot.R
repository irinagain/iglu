#' Plot scatterplot of metric comparisons
#'
#' @param data DataFrame with column names ("id", "time", and "gl").
#' @param metric1
#' @param metric2
#'
#' @return
#'
#' @export
#'

metric_scatter <- function(data, metric1 = 'adrr', metric2 = 'conga',
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
  if(metric1 %in% c('above_percent', 'below_percent', 'in_range_percent',
                    'summary_glu', 'quantile_glu', 'plot_glu')){
    stop('metric_scatter currently only supports metrics with a single column
         in their output.')
  }




}
}
