check_data_columns =  function(data, id = 'id', time = 'time', gl = 'gl', time_check = FALSE, tz = ""){
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
    if (time_check) {
      if (!lubridate::is.POSIXct(data$time)){ # Check if already in date format
        tr = as.character(data$time)
        data$time = as.POSIXct(tr, format='%Y-%m-%d %H:%M:%S', tz = tz)
      }
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


#' Interpolate glucose value on an equally spaced grid from day to day
#'
#' @inheritParams conga
#' @param data DataFrame object with column names "id", "time", and "gl". Should only be data for 1 subject. In case multiple subject ids are detected, the warning is produced and only 1st subject is used.
#' @param dt0 The time frequency for interpolation in minutes, the default will match the CGM meter's frequency (e.g. 5 min for Dexcom).
#' @param inter_gap The maximum allowable gap (in minutes) for interpolation. The values will not be interpolated between the glucose measurements that are more than inter_gap minutes apart. The default value is 45 min.
#'
#'
#' @return A list with
#' \item{gd2d}{A matrix of glucose values with each row corresponding to a new day, and each column corresponding to time}
#' \item{actual_dates}{A vector of dates corresponding to the rows of \code{gd2d}}
#' \item{dt0}{Time frequency of the resulting grid, in minutes}
#'
#' @export
#'
#' @examples
#'
#' CGMS2DayByDay(example_data_1_subject)
#'
CGMS2DayByDay <- function(data, dt0 = NULL, inter_gap = 45, tz = ""){

  # clean up environment
  gap = miss_tf = y = NULL
  rm(list = c("gap", "miss_tf", "y"))

  # complete.cases only works with POSIXct, not POSIXlt, so check for correct time format
  if (!lubridate::is.POSIXct(data$time)){
    tr = as.character(data$time)
    data$time = as.POSIXct(tr, format='%Y-%m-%d %H:%M:%S', tz = tz)
  }
  data = data[complete.cases(data),]

  ns = length(unique(data$id))

  if (ns > 1){
    id = NULL
    rm(list = c("id"))

    first = unique(data$id)[1]
    data = data %>% dplyr::filter(id == first)
    warning(paste("Data contains more than 1 subject. Only the first subject with id", first,  "is used for output."))
  }

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

  ### Check for time sorting
  if (min(timediff) < 0){
    warning(paste("The times for subject", unique(data$id), "are not in increasing order! The times will be sorted automatically."))
    index = order(tr)
    tr = tr[index]
    g = g[index]
    timediff = difftime(tr[timeindex], tr[timeindex - 1], units = "mins")
  }

  if (any(timediff == 0)){
    warning(paste("Subject", unique(data$id), "has repeated glucose measuremements at exactly the same times. Only the last one of repeated values is used in calculations."))
    index = which(timediff == 0)
    tr = tr[-index]
    g = g[-index]
    timediff = timediff[-index]
  }

  ### Automatically identify grid width dt0
  if (is.null(dt0)){
    dt0 = as.double(round(median(timediff, na.rm = TRUE)))
  }

  if (dt0 > inter_gap){
    stop(paste("Identified measurements frequency,", dt0, "min, is above the maximal interpolation gap of", inter_gap, "min."))
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

  ### Adjust so that there is no interpolation between values > inter_gap apart
  # first determine if such gaps exist and set the start+end points of such gaps to gap = TRUE
  gaps <- tibble::tibble(
    time = tr,
    timediff = c(as.numeric(timediff), 0),
    gap = (timediff > inter_gap) | (dplyr::lag(timediff, 1L, default = FALSE) > inter_gap)
  )
  # if any gaps exist, set any interpolated values in those times to NA
  if (any(gaps$gap)) {
    missing <- gaps %>%
      dplyr::filter(gap) %>%
      dplyr::mutate(
        # starts have timediff greater than inter_gap
        start = dplyr::if_else(timediff > inter_gap, time + 1, NA_real_),
        # ends are the ones where the time before is the gap start
        end = dplyr::if_else(dplyr::lag(timediff, 1L, default = FALSE) > inter_gap,
                             time - 1, NA_real_),
        # shift ends to match with their gap start time point
        end = c(end[2:dplyr::n()], NA)
      ) %>%
      dplyr::filter(!is.na(start)) # filter down to one entry per gap

    # create grid of interpolated time points that exist in each gap
    # rule = 1 means points outside of xmin/xmax will be NA (no extrapolation)
    intervals <- list(nrow(missing))
    for (i in 1:nrow(missing)) {
      grid = stats::approx(x = c(missing$start[i], missing$end[i]),
                           y = c(missing$start[i], missing$end[i]),
                           xout = time_out, rule = 1)$y
      intervals[[i]] <- grid[!is.na(grid)]
    }
    # unlist interpolated gaps and reconvert to posixct
    missing_times <- as.POSIXct(unlist(intervals), tz = tz, origin = "1970-01-01")
    # create missing df to join with original interpolation
    missing_df <- tibble::tibble(
      x = missing_times,
      miss_tf = TRUE
    )
    # join by time and set any times corresponding to gap (missing_times) to NA
    merged <- dplyr::left_join(new, missing_df, by = "x") %>%
      dplyr::mutate(
        y = ifelse(is.na(miss_tf), y, NA)
      )
    # if no gaps, then keep all interpolated times
  } else {
    merged <- new
  }

  # Next, from ti remove all the ones that are more than dt0 min away from t0
  gd2d = matrix(merged$y, nrow = ndays, byrow = TRUE)

  # Assign rownames that correspond to actual dates
  actual_dates = as.Date(minD) + lubridate::days(0:(ndays - 1))

  return(list(gd2d = gd2d, actual_dates = actual_dates, dt0 = dt0))
}
