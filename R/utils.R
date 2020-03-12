check_data_columns =  function(data, id = 'id', time = 'time', gl = 'gl'){
  if (is.vector(data)) {
    output = as.double(data)
    output = data.frame(gl = output,
               id = 1,
               time = NA_real_)
    attr(output, "is_vector") = TRUE
  } else {
    cols_in = c(id, time, gl) %in% names(data)
    if (!all(cols_in)) {
      s = setdiff(c(id, time, gl), names(data))
      msg = paste0("Columns: ", paste0(s, collapse = ", "),
                   " are not present in the data")
      stop(msg)
    }
    output = data[, c(id, time, gl), drop = FALSE]
    colnames(output) = c("id", "time", "gl")
    attr(output, "is_vector") = FALSE
  }
  return(output)
}

read_df_or_vec <- function(data, id = 'id', time = 'time', gl = 'gl'){
  if(class(data) %in% c('numeric', 'integer', 'double')){
    output = as.double(data)
  }
  else if(class(data) == 'data.frame'){
    indexes = which(names(data) %in% c(id, time, gl))
    if(length(indexes) < 3){
      stop("If passing a dataframe, make sure there are columns corresponding to id, time, and gl glucose values. At least one of
              these columns is missing or cannot be indentified by the supplied names.")
    }
    else if(length(indexes) > 3){
      stop("If passing a dataframe, make sure there is exactly 1 column corresponding to each of id, time, and gl glucose values, with no duplicates. At least
              one of id, time, or gl had multiple columns matching with supplied name.")
    }
    else {
      output = data[ , indexes]
      names(output) = c('id', 'time', 'gl')
    }
  }
  else {
    stop('Incompatible input type')
  }
  # output = switch(class(data), 'data.frame' = as.double(data$gl), 'numeric' = as.double(data), 'integer' = as.double(data), 'double' = as.double(data))
  return(output)
}

CGMS2DayByDay <- function(data){
  # This function creates interpolated data (using linear interpolation) for every dt0 times across, correctly handling missing data
  # Original code by Jacek Urbanek
  # Updated code (January 23, 2018) by Irina Gaynanova (as pertains to interval approximations)

  data = read_df_or_vec(data[complete.cases(data),])

  ### Get glycemic data
  g = data$gl

  ### estimate equally spaced time intervals starting from midnight
  t = data$time
  t = t[!is.na(g)]
  # Alternative time formatting (this was the issue before)
  tr = strptime(t, format = "%Y-%m-%d %H:%M:%S")
  tr = as.POSIXct(tr)

  timeindex = 2:length(tr)
  timediff = difftime(tr[timeindex], tr[timeindex - 1], units = "mins")
  dt0 = as.double(round(median(timediff, na.rm = T)))

  # What is the total number of days between last and first?
  ndays = ceiling(as.double(difftime(max(tr), min(tr), units = "days")) + 1)
  dti = c(0,rep(dt0, ndays * 24 * 60 /dt0))
  ti = cumsum(dti)

  # What is the data that we actually have?
  dt = c(as.double(lubridate::minute(tr[1])) + as.double(
    lubridate::hour(tr[1]))* 60, as.double(timediff))
  t0 = cumsum(dt)

  # Next, from ti remove all the ones that are more than dt0 min away from t0
  how_far_away = intervals::distance_to_nearest(ti, t0)
  ti_reduced = ti[how_far_away < dt0]

  # Extrapolate to the needed times only
  gi = approx(t0,g,ti_reduced)$y # g on equally spaced grid

  # Put NA to all other times
  gall = rep(NA, length(ti))
  gall[how_far_away < dt0] = gi

  gd2d = matrix(gall[-1], nrow = ndays, byrow = T)

  return(list(gd2d,dt0))
}

rename_glu_data <- function(data){

}

tsplot = function(data, hypo, hyper){
  gl = date_by_id = NULL
  rm(list = c("gl", "date_by_id"))
  data$date_by_id = as.POSIXct(data$time)
  ggplot2::ggplot(data = data, ggplot2::aes(x = date_by_id, y = gl, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(name = 'Date') +
    ggplot2::scale_y_continuous(name = 'Blood Glucose') +
    ggplot2::ggtitle(paste('Time Series plot for', unique(data$id)[1], sep = ' ')) +
    ggplot2::geom_hline(yintercept = hypo, color = 'red') +
    ggplot2::geom_hline(yintercept = hyper, color = 'red')
}

unsorted_lasagna = function(data){
  subjects = unique(data$id)
  limit = sum(data$id == subjects[1])
  if(length(subjects) >= 2){
    for(i in 2:length(subjects)){
      if(sum(data$id == subjects[i]) < limit){
        limit = sum(data$id == subjects[i])
      }
    }
  }
  H.mat = matrix(NA, nrow = length(subjects), ncol = limit)
  rownameslist = rep(NA, length(subjects))
  for(row in 1:length(subjects)){
    subject_subset = na.omit((data[data$id == subjects[row],]))
    subject_subset = subject_subset[1:limit,]
    H.mat[row, 1:limit] = subject_subset$gl
    rownameslist[row] = paste('S',row, sep = '')
  }
  rownames(H.mat) = rownameslist

  colnames(H.mat) = seq(ncol(H.mat))

  lasagnar::lasagna(H.mat, main = 'Unsorted lasagna plot', legend = T,
                    xlab = 'Measurement ordered by time', ylab = 'Subject')
}


rowsorted_lasagna = function(data){
  subjects = unique(data$id)
  limit = sum(data$id == subjects[1])
  if(length(subjects) >= 2){
    for(i in 2:length(subjects)){
      if(sum(data$id == subjects[i]) < limit){
        limit = sum(data$id == subjects[i])
      }
    }
  }
  H.mat = matrix(NA, nrow = length(subjects), ncol = limit)
  rownameslist = rep(NA, length(subjects))
  for(row in 1:length(subjects)){
    subject_subset = na.omit((data[data$id == subjects[row],]))
    subject_subset = subject_subset[1:limit,]
    H.mat[row, 1:limit] = subject_subset$gl
    rownameslist[row] = paste('S',row, sep = '')
  }
  rownames(H.mat) = rownameslist

  colnames(H.mat) = seq(ncol(H.mat))

  lasagnar::lasagna(
    lasagnar::wr.disc(H.mat),
    main = 'Within-row sorted lasagna plot',
    legend = TRUE, xlab = 'Measurement ordered by gl value, increasing',
    ylab = 'Subject')
}

