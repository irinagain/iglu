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
    output = data[ , which(names(data) %in% c(id, time, gl))]
    names(output) = c('id', 'time', 'gl')
  }
  # output = switch(class(data), 'data.frame' = as.double(data$gl), 'numeric' = as.double(data), 'integer' = as.double(data), 'double' = as.double(data))
  return(output)
}

data_to_gl_format <- function(){

}
rate_of_change <- function(){

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
  tr = as.POSIXct(t)

  timeindex = 2:length(tr)
  timediff = difftime(tr[timeindex], tr[timeindex - 1], units = "mins")
  dt0 = as.double(round(median(timediff, na.rm = T)))

  # What is the total number of days between last and first?
  ndays = ceiling(as.double(difftime(max(tr), min(tr), units = "days")) + 1)
  dti = c(0,rep(dt0,ndays * 24 * 60 /dt0))
  ti = cumsum(dti)

  # What is the data that we actually have?
  dt = c(as.double(minute(tr[1])) + as.double(hour(tr[1]))* 60, as.double(timediff))
  t0 = cumsum(dt)

  # Next, from ti remove all the ones that are more than dt0 min away from t0
  how_far_away = distance_to_nearest(ti, t0)
  ti_reduced = ti[how_far_away < dt0]

  # Extrapolate to the needed times only
  gi = approx(t0,g,ti_reduced)$y # g on equally spaced grid

  # Put NA to all other times
  gall = rep(NA, length(ti))
  gall[how_far_away < dt0] = gi

  gd2d = matrix(gall[-1], nrow = ndays, byrow = T)

  return(list(gd2d,dt0))
}
