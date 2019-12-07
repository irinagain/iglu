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

unsorted_lasagna = function(data, hypo, hyper){
  subjects = unique(data$id)
  H.mat = matrix(NA, nrow = length(subjects), ncol = 1000)
  rownameslist = rep(NA, length(subjects))
  for(row in 1:length(subjects)){
    subject_subset = na.omit((data[data$id == subjects[row],]))
    subject_subset = subject_subset[1:1000,]
    H.mat[row, 1:1000] = subject_subset$gl
    rownameslist[row] = paste('S',row, sep = '')
  }
  rownames(H.mat) = rownameslist

  colnames(H.mat) = seq(ncol(H.mat))

  lasagnar::lasagna(H.mat, main = 'Unsorted lasagna plot', legend = T,
                    xlab = 'Measurement ordered by time', ylab = 'Subject')
}


