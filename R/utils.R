convert_glu_measurement_type <- function(){
}

data_to_gl_format
rate_of_change <- function(){

}

CGMS2DayByDay <- function(data){
  # This function creates interpolated data (using linear interpolation) for every dt0 times across, correctly handling missing data
  # Original code by Jacek Urbanek
  # Updated code (January 23, 2018) by Irina Gaynanova (as pertains to interval approximations)


  library("lubridate")
  library(intervals)

  data = data[complete.cases(data),]

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
