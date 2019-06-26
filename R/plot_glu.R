
#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_glu <- function(data){
  gl_by_id = as.double(data$gl)
  # ***
  date_by_id = as.POSIXct(data$time)
  ggplot2::ggplot(data = data, aes(x = date_by_id, y = gl_by_id, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(name = 'Date')
      }
