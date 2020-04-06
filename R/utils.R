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


#' Interpolate on an equally spaced grid from day to day, allows cross-day comparisons at the same time
#'
#' @inheritParams mean_glu
#' @param dt0 The time frequency for interpolation in minutes, the default will match the CGM meter's frequency (e.g. 5 min for Dexcom).
#' @param inter_gap The maximum allowable gap (in minutes) for interpolation. The values will not be interpolated between the glucose measurements thare are more than inter_gap minutes apart. The default value is 45 min.
#'
#' @return
#'
#' @examples
#'
#' CGMS2DayByDay(example_data_1_subject)
#'


CGMS2DayByDay <- function(data, dt0 = NULL, inter_gap = 45, tz = ""){

  data = data[complete.cases(data),]

  ### Get glycemic data
  g = as.numeric(data$gl) # in case it's not

  ### Get time data
  if (lubridate::is.POSIXct(data$time)){ # Check if already in date format
    tr = data$time
  }else{
    tr = as.character(data$time)
    tr = as.POSIXct(tr, format='%Y-%m-%d %H:%M:%S', tz = tz)
    # Check if any NAs from conversion, this happens if wrong time format (e.g. 25:00:00) or wrong time zone which will affect daylight savings time
    if (any(is.na(tr))){
      warning(paste("During time conversion,", sum(is.na(tr)), "values were set to NA. Check the correct time zone specification."))
      g = g[!is.na(tr)]
      tr = tr[!is.na(tr)]
    }
  }

  timeindex = 2:length(tr)
  timediff = difftime(tr[timeindex], tr[timeindex - 1], units = "mins")

  ### Check for time soring
  if (min(timediff) < 0){
    stop(paste("The times for subject", unique(data$id), "are not in increasing order!"))
  }

  ### Automatically identify grid width dt0
  if (is.null(dt0)){
    dt0 = as.double(round(median(timediff, na.rm = T)))
  }

  if (dt0 > inter_gap){
    stop(paste("Identified measurements frequency,", dt0, "min, is above the maximal interpolation gap of", inter_gap, "min!"))
  }

  ### Check for misaligned grid length dt0 across days, and adjust if needed
  if (1440 %% dt0 != 0){
    if (dt0 > 20){ # Larger grid lengths are set to 20 min
      dt0 = 20
    }else{ # Smaller grid lengths are rounded so that they are divisible by 5 min
      remainder = dt0 %% 5
      if (remainder > 2){
        dt0 = dt0 + 5 - remainder
      }else{
        dt0 = dt0 - remainder
      }
    }
  }

  ### Create ideal grid to interpolate over, from minimal to maximal day
  ndays = ceiling(as.double(difftime(max(tr), min(tr), units = "days")) + 1)
  dti = rep(dt0, ndays * 24 * 60 /dt0)
  dti_cum = cumsum(dti)
  dti_cum = lubridate::minutes(dti_cum)

  # Set up starting point at 00:00am on the first day
  minD = min(tr) # 1st day of measurements
  lubridate::hour(minD) = 0 # set to 00am
  lubridate::minute(minD) = 0 # set to 00:00
  lubridate::second(minD) = 0 # set to 00:00:00

  # Create a set of time points for interpolation
  time_out = minD + dti_cum

  ### Interpolate on the ideal grid
  new <- as.data.frame(stats::approx(x = tr, y = g, xout = time_out))

  ### Adjust to that there is no interpolation between values > inter_gap appart
  ### Thanks to weather_interp function from weathercan R package for inspiration
  inter_gap <- lubridate::minutes(inter_gap)
  timediff <- lubridate::minutes(round(timediff))
  which_gap <- tr[c(timediff > inter_gap, FALSE)]
  missing <- lubridate::interval(which_gap + 1, which_gap + timediff[timediff > inter_gap] - 1)
  missing <- vapply(new$x, FUN.VALUE = TRUE, FUN = function(x, missing) {
    any(lubridate::`%within%`(x, missing))
  }, missing = missing)
  new$y[missing] <- NA

  # Next, from ti remove all the ones that are more than dt0 min away from t0
  gd2d = matrix(new$y, nrow = ndays, byrow = T)

  # Assign rownames that correspond to actual dates
  actual_dates = as.Date(minD) + lubridate::days(0:(ndays - 1))

  return(list(gd2d = gd2d, actual_dates = actual_dates, dt0 = dt0))
}

# rename_glu_data <- function(data){
#
# }

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

unsorted_lasagna = function(data, limits = c(50, 400), midpoint = 125){
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

  #lasagnar::lasagna(H.mat, main = 'Unsorted lasagna plot', legend = T, xlab = 'Measurement ordered by time', ylab = 'Subject')
  H.mat_melt = reshape::melt(H.mat)
  p = ggplot(data=H.mat_melt,aes(x = X2, y = X1, fill = value)) + geom_tile()+scale_fill_gradient2(low = "blue", high = "red", limits = limits, midpoint = midpoint)+ ylab("Subject") + ggtitle("Unsorted lasagna plot") + xlab("Measurement ordered by time")
  print(p)

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

