# helper function for checking time format
time_check <- function(time, name, tz){
  if (!lubridate::is.POSIXct(time)){
    # convert to character and then posixct using tz
    tr = as.character(time)
    out <- as.POSIXct(tr, format='%Y-%m-%d %H:%M:%S', tz = tz)
    # if all na, stop and message
    if (all(is.na(out))){
      stop(paste0("During time conversion all ", name, " set to NA, please check input"))
      # else if not all but some coerced to NA, warn and remove NAs
    } else if (any(is.na(out))){
      warning(paste0("During time conversion some, ", name, " set to NA, please check input"))
      out = out[!is.na(out)]
    }
    # else if already posixct, return original time back
  } else{
    out = time
  }
  return(out)
}

# helper to adjust mealtimes to match closest CGM time (optional)
adj_mtimes <- function(data, mealtime, dt0) {

  # where mealtime is time of one specific meal, find diffs from cgm times
  timediffs <- abs(difftime(data$time, mealtime, units = "mins"))
  # if within one meter measurement away then map
  ## need dt0 I think
  if (any(timediffs <= dt0)) {
    # remap x time as minimum time difference cgm time
    x <- data$time[which.min(timediffs)]
  }
  # if not within one measurement then return original (won't map to the data later)
  else {
    x = mealtime
  }

  # return adjusted mealtime
  return(x)
}

meal_metrics_single <- function (data, mealtimes, before_win, after_win,
                                 recovery_win, interpolate, adjust_mealtimes,
                                 dt0, inter_gap, tz) {

  id = meal = mealtime = idx = period = peak = base = recover = deltag = basereco =
    basegl = peakgl = recovergl = peaktime = recovertime = NULL
  rm(list = c("id", "meal", "mealtime", "idx", "period", "peak", "base", "recover",
              "deltag", "basereco", "basegl", "peakgl", "recovergl", "peaktime", "recovertime"))

  # if id is not in mealtimes data
  if (!(unique(data$id) %in% unique(mealtimes$id))) {
    # message no mealtimes found for specific subject
    message(paste0("No mealtimes found for subject: ", unique(data$id)))
    # return compatible tibble with NA for all missing values
    out <- tibble::tibble(id = unique(data$id), time = as.POSIXct(NA, tz = tz), meal = NA_character_,
                          deltag = NA_real_, deltat = NA_real_, basereco = NA_real_,
                          basegl = NA_real_, peakgl = NA_real_, recovergl = NA_real_,
                          peaktime = as.POSIXct(NA, tz = tz), recovertime = as.POSIXct(NA, tz = tz))
    return(out)
  }

  # check if data and mealtimes are in same timezone, warn if not
  if (lubridate::tz(data$time) != lubridate::tz(mealtimes$mealtime)) {
    lubridate::tz(data$time) <- lubridate::tz(mealtimes$mealtime)
    warning("Data and meals timezone do not match, mealtimes timezone chosen for all")
  }

  ## interpolate data (important especially for baseline recovery)
  # make optional since interpolation can be time consuming
  if (interpolate) {
    # use cgms2daybyday to interpolate over uniform grid
    data_ip <- CGMS2DayByDay(data, dt0 = dt0, inter_gap = inter_gap, tz = tz)
    # define dt0 in case needed for mealtimes adjustment
    dt0 <- data_ip$dt0
    # find first day and number of days
    # must set tz to match data/mealtimes tz
    data_tz <- lubridate::tz(data$time)
    day_one = lubridate::as_datetime(data_ip[[2]][1], tz = data_tz)
    ndays = length(data_ip[[2]])
    # generate grid times by starting from day one and cumulatively summing
    time_ip =  day_one + lubridate::minutes(cumsum(
      # replicate dt0 by number of measurements (total minutes/dt0)
      rep(data_ip$dt0, ndays * 24 * 60 /data_ip$dt0)))
    # change data into id, interpolated times, interpolated glucose (t to get rowwise)
    data <- data %>%
      dplyr::reframe(id = id[1], time = time_ip, gl = as.vector(t(data_ip$gd2d))) %>%
      # remove leading and trailing NAs
      dplyr::filter(!is.na(gl))
  } else {
    data = data
    timediff <- difftime(data$time[2:length(data$time)],
                         data$time[1:(length(data$time)-1)], units = "mins")
    # if not interpolate, find median frequency for optional adjusting mealtimes
    dt0 <- as.double(round(median(timediff, na.rm = TRUE)))
  }

  meals_single = mealtimes %>%
    # select only meals for this groups id
    dplyr::filter(id == unique(data$id))

  ### trouble if mealtime doesn't exactly correlate with cgm times

  # set adjust mealtimes to true to attempt to align mealtimes and cgm
  if (adjust_mealtimes) {

    # rowwise summary to adjust each meal
    meals_single <- meals_single %>%
      dplyr::rowwise() %>%
      # now mealtime should match with nearby cgm time or be NA
      dplyr::summarise(id = id, meal = meal, mealtime = adj_mtimes(data, mealtime, dt0))
  }

  ## if no mealtimes match with cgm times (likely due to not interpolating or adjusting)
  # then exit out and warn
  if (!any(meals_single$mealtime %in% data$time)) {
    out = meals_single %>%
      dplyr::mutate(deltag = NA_real_, deltat = NA_real_, basereco = NA_real_,
                    basegl = NA_real_, peakgl = NA_real_, recovergl = NA_real_,
                    peaktime = as.POSIXct(NA, tz = tz), recovertime = as.POSIXct(NA, tz = tz)) %>%
      dplyr::select(id, time = mealtime, meal, deltag, deltat, basereco)

    warning("No meals match with recorded CGM timestamps.
            Please try running with interpolate = TRUE and/or adjust_mealtimes = TRUE")

    return(out)
  }

  if (any(!(meals_single$mealtime %in% data$time))) {
    non_match = meals_single %>%
      dplyr::filter(!(mealtime %in% data$time)) %>%
      dplyr::mutate(
        deltag = NA_real_, deltat = NA_real_, basereco = NA_real_,
        basegl = NA_real_, peakgl = NA_real_, recovergl = NA_real_,
        peaktime = as.POSIXct(NA, tz = tz), recovertime = as.POSIXct(NA, tz = tz)
      ) %>%
      dplyr::select(id, time = mealtime, meal, deltag, deltat, basereco, basegl, peakgl, recovergl, peaktime, recovertime)

    warning("Some meals don't match with CGM readings. Mealtimes returned with NA for metric values")

    # Filter to only matching ones
    meals_single = meals_single[meals_single$mealtime %in% data$time, ]
    NAflag = TRUE
  }else{
    NAflag = FALSE
  }

  # Assign each meal a unique id
  meals_single = meals_single %>%
    dplyr::mutate(mealid = 1:n())

  # find total window time
  total_win <- before_win + after_win + recovery_win
  meals_annotated = meals_single %>%
    # create start user specified number of hours before meal
    dplyr::mutate(start = mealtime - before_win*60*60) %>%
    dplyr::rowwise() %>%
    # for each meal window, create 4 periods
    dplyr::reframe(mealid = mealid,
                   meal = meal,
                   # time is total window (secondly)
                   time = start + lubridate::seconds(0:(total_win*60*60)),
                   # each window now has 4 periods: before, meal, after, recovery
                   period = c(rep("before", before_win*60*60), "meal",
                              rep("after", after_win*60*60),
                              rep("recovery", recovery_win*60*60)))

  # combine cgm data with mealtimes
  # if a given time corresponds to more than one meal, all possible
  # combos are returned
  out <- data %>%
    dplyr::left_join(meals_annotated, by = "time") %>%
    # make sure time is ascending
    dplyr::arrange(time)

  list_all <- list()
  # list types of meals present for this subject
  # mealtypes <- unique(out$meal[!is.na(out$meal)])
  mealids <- unique(out$mealid[!is.na(out$mealid)])

  for (i in 1:length(mealids)) {
    list_all[[i]] <- out %>%
      # filter down to NAs and specified meals
      # removes overlap from other meal windows
      dplyr::filter(is.na(mealid) | mealid == mealids[i]) %>%
      # index by each individual meal + intervening - NA, meal, NA, etc.
      dplyr::mutate(idx = rep(1:length(rle(mealid)[[1]]), rle(mealid)[[1]])) %>%
      # filter down to only meals, will have unique idx for each meal
      dplyr::filter(!is.na(mealid)) %>%
      dplyr::group_by(idx) %>%
      # calculate numbers necessary for adj_metrics calculation
      # base is average of gl values before
      dplyr::mutate(base = mean(gl[period == "before"], na.rm = TRUE),
                    # peak is max gl after meal
                    peak = max(gl[period == "after"], na.rm = TRUE),
                    # recovery is time one hour after peak
                    recover = time[period == "after"][which.max(gl[period == "after"])] +
                      1*60*60) %>%
      dplyr::reframe(id = id[1], meal = meal[1], mealid = mealid[1],
                     mealtime = time[period == "meal"],
                     basegl = base[1], peakgl = peak[1],
                     recovergl = ifelse(recover[1] %in% time, gl[time == recover[1]], NA),
                     peaktime = recover[1] - 1*60*60, recovertime = recover[1],
                     # deltag is change in gl from baseline to peak
                     deltag = peak[1] - base[1],
                     # deltat is time to peak (peak time is 1 hr before recovery)
                     # converted to numeric for later use (and to match default NAs)
                     deltat = as.numeric(difftime(recover[1] - 1*60*60, time[period == "meal"],
                                                  units = "mins")),
                     # baseline recovery is gl change from peak to 1hr after/deltag
                     # only calculate basereco if recovery time is within 4 hr window
                     basereco = ifelse(recover[1] %in% time,
                                       (peak[1] - gl[time == recover[1]])/deltag[1], NA)) %>%
      dplyr::select(id, time = mealtime, meal, deltag, deltat, basereco, basegl, peakgl, recovergl, peaktime, recovertime)
  }

  # for meals found in data
  out <- do.call(rbind, list_all)

  # if any of the meals didn't match, save them as NAs
  if (NAflag){
    out = rbind(out, non_match)
  }


  # Resort to match original order
  out <- out[order(out$time), ]


  return(out)
}

#' Calculate Meal Metrics
#'
#' @description
#' The function meal_metrics calculates three simple glucose meal metrics
#'
#' @usage meal_metrics(data, mealtimes, before_win = 1, after_win = 3,
#' recovery_win = 1, interpolate = TRUE, adjust_mealtimes = TRUE, dt0 = NULL,
#' inter_gap = 45, tz = "", glucose_times = FALSE)
#'
#' @inheritParams CGMS2DayByDay
#'
#' @param mealtimes Either a vector of mealtimes, corresponding
#' to data being from a single subject, OR a dataframe with at least 2 columns labeled
#' id and mealtime. Optionally the mealtimes dataframe can include a column labeled meal,
#' giving the meal type (helps to compensate for overlapping meals)
#' @param before_win integer specifying number of hours to extend window before meal
#' @param after_win integer specifying number of hours to extend window after meal
#' @param recovery_win interger specifying number of hours for recovery beyond after window
#' @param interpolate Boolean to indicate if CGM data should be interpolated or not.
#' Default set to FALSE due to time intensive nature of interpolation. Parameters dt0,
#' inter_gap, and tz will only be used if interpolate is set to TRUE.
#' @param adjust_mealtimes Boolean to indicate if function should attempt to align
#' mealtimes with CGM data times. This is important if mealtimes and CGM data times
#' are not exactly aligned, because the function will return NA's for mealtimes
#' that don't match with a corresponding CGM time stamp.
#' @param glucose_times Boolean to indicate if underlying glucose and times should be returned
#' - e.g. baseline glucose value used to calculate \eqn{Delta G}. Intended for use in plotting function
#'
#' @return By default tibble object with 6 columns will be returned: id, time, meal, deltag,
#' deltat, and basereco. If glucose_times = TRUE then 5 more columns are returned
#' for baseline glucose (basegl), peak glucose (peakgl), recover glucose (recovergl),
#' peak timestamp (peaktime), and recovery timestamp (recovertime)
#'
#' @export
#'
#' @details A tibble object is returned with three metrics calculated for each mealtime.
#' The last three columns of the output correspond to the three metrics: deltag
#' refers to \eqn{\Delta G}, deltat is \eqn{\Delta T}, and basereco is \% Baseline recovery.
#' If no meal column is given in the original data, one will be automatically generated
#' with a unique number for each meal.
#'
#' @seealso plot_meals()
#'
#' @references
#' Service, F. John. (2013) Glucose Variability, \emph{Diabetes}
#' \strong{62(5)}: 1398-1404, \doi{10.2337/db12-1396}
#'
#' @examples
#' data(example_data_hall)
#' data(example_meals_hall)
#' meal_metrics(example_data_hall, example_meals_hall)
#'


meal_metrics <- function (data, mealtimes, before_win = 1, after_win = 3,
                          recovery_win = 1, interpolate = TRUE,
                          adjust_mealtimes = TRUE, dt0 = NULL, inter_gap = 45,
                          tz = "", glucose_times = FALSE) {

  id = meal = mealtime = idx = period = peak = base = recover = deltag = basereco = NULL
  rm(list = c("id", "meal", "mealtime", "idx", "period", "peak", "base", "recover",
              "deltag", "basereco"))

  # check with iglu internal function
  data = check_data_columns(data, time_check = TRUE, tz = tz)
  tz = lubridate::tz(data$time) #reset timezone based on what is in the data

  ## need time format check for mealtimes

  # check mealtimes data
  if (is.vector(mealtimes)) {
    warning("Vector of mealtimes entered. Mealtimes assumed to apply to all subject(s) in the data.")
    # check if posixct and force if not
    mealtimes <- time_check(mealtimes, name = "mealtimes", tz = tz)

    # Check for missing mealtime
    if (any(is.na(mealtimes))){
      message(paste0("Some meal times are NA. Those records are removed."))
      mealtimes <- mealtimes[!is.na(mealtimes)]
    }

    # create tibble of id and mealtime to be used in meal_metrics_single
    # repeat id a mealtime # of times for each id
    ids <- unique(data$id)
    mt_df <- tibble::tibble(id = rep(ids, rep(length(mealtimes), length(ids))),
                            meal = 1:length(id),
                            mealtime = rep(mealtimes, length(ids)))
    mealtimes <- mt_df
  }
  # follow check_data_columns format to check for mealtimes columns
  else {
    # id and mealtime required if dataframe is given
    cols_in = c("id", "mealtime") %in% names(mealtimes)
    if (!all(cols_in)) {
      s = setdiff(c("id", "mealtimes"), names(data))
      msg = paste0("Column(s): ", paste0(s, collapse = ", "), " are not present in the mealtimes data")
      stop(msg)
    }
    # check time format
    mealtimes$mealtime <- time_check(mealtimes$mealtime, "mealtimes", tz = tz)

    # Check for missing mealtime
    if (any(is.na(mealtimes$mealtime))){
      message(paste0("Some meal times are NA. Those records are removed."))
      mealtimes <- mealtimes %>% dplyr::filter(!is.na(mealtime))
    }

    # if lacking meal type in mealtimes, message
    if (!("meal" %in% names(mealtimes))) {
      message(paste0("Meal types unspecified. Each mealtime will be assigned a ",
                     "meal number to assist in calculation"))
      # auto add meal column to deal with any potential time overlaps
      mealtimes <- mealtimes %>%
        dplyr::mutate(meal = 1:length(mealtimes$mealtime))
    }
  }
  # at this point, mealtimes should be a df with at least id, time, meal cols

  out <- data %>%
    dplyr::group_by(id) %>%
    # calculate meal metrics for each subject
    dplyr::reframe(meal_metrics_single(data.frame(id, time, gl), mealtimes = mealtimes,
                                       before_win = before_win, after_win = after_win,
                                       recovery_win = recovery_win, interpolate = interpolate,
                                       adjust_mealtimes = adjust_mealtimes, dt0 = dt0,
                                       inter_gap = inter_gap, tz = tz)) %>%
    dplyr::ungroup()

  return(out)
}
