tsplot = function(data, hypo, hyper){
  date_by_id = as.POSIXct(data$time)
  ggplot2::ggplot(data = data, ggplot2::aes(x = date_by_id, y = data$gl, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(name = 'Date') +
    ggplot2::scale_y_continuous(name = 'Blood Glucose') +
    ggplot2::ggtitle(paste('Time Series plot for', unique(data$id)[1], sep = ' ')) +
    ggplot2::geom_vline(xintercept = hypo, color = 'red') +
    ggplot2::geom_vline(xintercept = hyper, color = 'red')
}
