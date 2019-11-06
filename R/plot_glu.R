#' Plot time series of glucose measurements
#'
#' @param data DataFrame with column names ("id", "time", and "gl"),
#' or vector of glucose values as integer, numeric, or double.
#'
#' @return
#'
#' @export
#'
#' @examples
#' plot_glu(example_data_1_subject)
#' plot_glu(example_data_5_subject)

plot_glu <- function(data){
  gl_by_id = read_df_or_vec(data)
  if(is.data.frame(data) && length(unique(data$id))== 1){
    glu_tsplot <- function(data){
      date_by_id = as.POSIXct(data$time)
      ggplot2::ggplot(data = data, ggplot2::aes(x = date_by_id, y = gl_by_id, group = 1)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_datetime(name = 'Date') +
        ggplot2::scale_y_continuous(name = 'Blood Glucose') +
        ggplot2::ggtitle(paste('Time Series plot for', unique(data$id), sep = ' '))
    }
    glu_tsplot(data)
  } else if(is.data.frame(data) && nrow(data) != 1){
    glu_lasagnaplot <- function(data){
      subjects = unique(data$id)
      H.mat = matrix(NA, nrow = length(subjects), ncol = 1000)
      rownameslist = rep(NA, length(subjects))
      min_obs = min(table(example_data_5_subject$id))
      for(row in 1:length(subjects)){
        subject_subset = na.omit((data[data$id == subjects[row],]))
        subject_subset = subject_subset[1:min_obs,]
        H.mat[row, 1:1000] = subject_subset$gl
        rownameslist[row] = paste('S',row, sep = '')
    }
    #rownames(H.mat) = subjects
      rownames(H.mat) = rownameslist

      colnames(H.mat) = seq(ncol(H.mat))
      names(dimnames(H.mat)) = c('Subject id','Time interval')

      lasagnar::lasagna(H.mat, main = 'Initial lasagna plot', legend = T,
            xlab = 'Measurement ordered by time', ylab = 'Subject')
      lasagnar::lasagna(lasagnar::wr.disc(H.mat), main = 'Within-row sorted lasagna plot',
              legend = T, xlab = 'Measurement ordered by gl value, increasing',
              ylab = 'Subject')
      }
      glu_lasagnaplot(data)
  } else{
    hist(gl_by_id)
  }
}
