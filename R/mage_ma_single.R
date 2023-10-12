#' Calculates Mean Amplitude of Glycemic Excursions (see "mage")
#'
#' @description This function is an internal function used by "mage". The function will calculate the Mean Amplitude of Glycemic Excursions (MAGE) on \strong{all} the values of the inputted data set. To calculate separate MAGE values for a group of subjects, use the "mage" function.
#'
#' @author Nathaniel Fernandes
#' @details See "mage".
#'
#' @inheritParams CGMS2DayByDay
#' @param short_ma Integer for period length of the short moving average. Must be positive and less than "long_ma", default value is 5. (Recommended <15)
#' @param long_ma Integer for period length for the long moving average, default value is 32. (Recommended >20)
#' @param type One of "plus", "minus", "auto" (Default: auto). Algorithm will either calculate MAGE+ (nadir to peak), MAGE- (peak to nadir), or automatically choose based on the first countable excursion.
#' @param plot Boolean. Returns ggplot if TRUE.
#' @param title Title for the ggplot. Defaults to "Glucose Trace - Subject [ID]"
#' @param xlab Label for x-axis of ggplot. Defaults to "Time"
#' @param ylab Label for y-axis of ggplot. Defaults to "Glucose Level"
#' @param show_ma Whether to show the moving average lines on the plot or not
#'
#' @return The numeric MAGE value for the inputted glucose values or a ggplot if \code{plot = TRUE}
#'
#' @export
#'
#' @examples
#' data(example_data_1_subject)
#' mage_ma_single(
#'    example_data_1_subject,
#'    short_ma = 4,
#'    long_ma = 24,
#'    type = 'plus')
#'
#' mage_ma_single(
#'    example_data_1_subject,
#'    inter_gap = 300)
#'
#' mage_ma_single(
#'    example_data_1_subject,
#'    plot=TRUE,
#'    title="Patient X",
#'    xlab="Time",
#'    ylab="Glucose Level (mg/dL)",
#'    show_ma=FALSE)

mage_ma_single <- function(data, short_ma = 5, long_ma = 32,
                           return_type = c('num', 'df'), return_ = c('auto', 'plus', 'minus'),
                           plot = FALSE, dt0 = NULL, inter_gap = 45, max_gap = 90,
                           tz = "", title = NA, xlab = NA, ylab = NA, show_ma = FALSE) {

  ## 0. Calculates MAGE on 1 segment of CGM trace
  mage_atomic <- function(.data) {
    nmeasurements = list_cross = types = count = crosses = num_extrema = minmax = indexes = s1 = s2 = standardD = heights = nadir2peak = tp_indexes = NULL
    rm(list=c('nmeasurements', 'list_cross', 'types', 'count', 'crosses', 'num_extrema', 'minmax', 'indexes', 's1', 's2', 'standardD', 'heights', 'nadir2peak', 'tp_indexes'))

    if (all(is.na(.data$gl))) {
      return(data.frame(start=head(.data$time, 1), end=tail(.data$time, 1), mage=NA, pm=NA, first_excursion=NA))
    }

    nmeasurements = nrow(.data)

    if (nmeasurements < 7){
      warning("The number of measurements is too small for MAGE calculation.")
      return(data.frame(start=head(.data$time, 1), end=tail(.data$time, 1), mage=NA, pm=NA, first_excursion=NA))
    } else if (nmeasurements < long_ma){
      warning("The total number of measurements is smaller than the long moving average. ")
      return(data.frame(start=head(.data$time, 1), end=tail(.data$time, 1), mage=NA, pm=NA, first_excursion=NA))
    }

    # 2c. Calculate the moving average values
    .data <- .data %>%
      dplyr::mutate(MA_Short = zoo::rollapply(gl, width = short_ma, FUN = mean,
                                              align = 'right', fill = NA, na.rm = TRUE),
                    MA_Long  = zoo::rollapply(gl, width = long_ma, FUN = mean,
                                              align = 'right', fill = NA, na.rm = TRUE)) %>%
      # fill in leading NA's from using rolling mean
      dplyr::mutate(MA_Short = replace(MA_Short, 1:short_ma, MA_Short[short_ma]),
                    MA_Long  = replace(MA_Long, 1:long_ma, MA_Long[long_ma]),
                    DELTA_SHORT_LONG = MA_Short - MA_Long)


    # 2d. Create a preallocated list of crossing point ids & type
    idx = as.numeric(rownames(.data))
    types = list2env(list(REL_MIN=0, REL_MAX=1))

    list_cross <- list("id" = rep.int(NA, nmeasurements), "type" = rep.int(NA, nmeasurements)) # TODO: why does this need to be pre-allocated in the 1st place?

    # always add 1st point
    list_cross$id[1] <- idx[1]
    list_cross$type[1] <- ifelse(.data$DELTA_SHORT_LONG[1] > 0, types$REL_MAX, types$REL_MIN)
    count = 2

    for(i in 2:length(.data$DELTA_SHORT_LONG)) {
      if(
        !is.na(.data$gl[i]) && !is.na(.data$gl[i-1]) &&
        !is.na(.data$DELTA_SHORT_LONG[i]) && !is.na(.data$DELTA_SHORT_LONG[i-1])
      ) {
        # crossing point if DELTA changes sign or curr DELTA is 0
        if(.data$DELTA_SHORT_LONG[i] * .data$DELTA_SHORT_LONG[i-1] < 0 || (.data$DELTA_SHORT_LONG[i] == 0 && .data$DELTA_SHORT_LONG[i-1] != 0)) {
          list_cross$id[count] <- idx[i]
          list_cross$type[count] = ifelse(.data$DELTA_SHORT_LONG[i] < .data$DELTA_SHORT_LONG[i-1], types$REL_MIN, types$REL_MAX)
          count <- count + 1
        }
      }
    }

    # Add last point to capture excursion at end
    list_cross$id[count+1] <- tail(idx, 1)
    list_cross$type[count+1] <- ifelse(.data$DELTA_SHORT_LONG[nrow(.data)] > 0, types$REL_MAX, types$REL_MIN)

    # Filter for non-na values then combine into a table
    list_cross$id <- list_cross$id[!is.na(list_cross$id)] # TODO: remove preallocation & get rid of this
    list_cross$type <- list_cross$type[!is.na(list_cross$type)]

    crosses <- do.call(cbind.data.frame, list_cross)

    # 2e. Calculate min and max glucose values from ids and types in crosses + store indexes for plotting later
    num_extrema = nrow(crosses)-1
    minmax <- rep(NA_real_, num_extrema)
    indexes <- rep(NA_real_, num_extrema)
    for(i in 1:num_extrema) {
      s1 <- crosses[i,1]    # indexes of crossing points
      s2 <- crosses[i+1,1]

      if(crosses[i, "type"] == types$REL_MIN) {
        minmax[i] <- min(.data[as.character(s1:s2), ]$gl, na.rm = TRUE)
        indexes[i] <- which.min(.data[as.character(s1:s2), ]$gl)+s1-1
      } else {
        minmax[i] <- max(.data[as.character(s1:s2), ]$gl, na.rm = TRUE)
        indexes[i] <- which.max(.data[as.character(s1:s2), ]$gl)+s1-1
      }
    }

    ##########################################
    ##### New Mage Excursion Elimination #####
    ##########################################

    differences = t(outer(minmax, minmax, `-`))
    standardD <- sd(.data$gl, na.rm = TRUE)
    N = length(minmax)

    # MAGE+
    mage_plus_heights = c()
    mage_plus_tp_pairs = c()
    i = prev_i = 1

    while (i <= N) {
      delta = differences[prev_i:i,i] # minmax_i - minmax_j st. i >= j

      max_v = max(delta) # max does left-side accumulation
      j = which.max(delta) + prev_i - 1 # add `prev_i - 1` to adjust for offset since only subsetting prev_i:i

      if (max_v >= standardD) {
        # we've found left excursion, nadir2peak > sd
        for (k in i:N) {
          if (differences[i, k] <= -1*standardD || k == N) {
            # we've found the first large enoough right side || unmatched left excursion
            mage_plus_heights = append(mage_plus_heights, max_v)
            mage_plus_tp_pairs = append(mage_plus_tp_pairs, c(j, i))

            prev_i = i
            i = k
            break
          }
        }
      }

      else {
        i = i + 1
      }
    }

    # MAGE-
    mage_minus_heights = c()
    mage_minus_tp_pairs = c()
    i = prev_i = 1

    while (i <= N) {
      delta = differences[prev_i:i, i] # minmax_i - minmax_j st. i >= j

      min_v = min(delta) # min does left-side accumulation
      j = which.min(delta) + prev_i - 1

      # print(paste0(i min_v))
      if (min_v <= -1*standardD) {
        for (k in i:N) {
          if (differences[i, k] >= standardD || k == N) {
            mage_minus_heights = append(mage_minus_heights, min_v)
            mage_minus_tp_pairs = append(mage_minus_tp_pairs, c(j, i))

            prev_i = i
            i = k
            break
          }
        }
      }

      else {
        i = i + 1
      }
    }

    ## 3. Calculate MAGE
    # 3a. Standard deviation

    # 3b. Calculate the valid excursion heights
    #     - heights: vector that will contain final heights
    #     - peak2nadir: measure excursion peak to nadir. unassigned (-1), false (0), true (1)
    #     - n: a counter that helps iterate through the turning points
    #     - tp_indexes: vector that will contain tp from valid excursions
    heights <- numeric()
    type <- match.arg(type, c('auto', 'plus', 'minus'))

    nadir2peak <- if(type == "plus") {
      1
    } else if (type == "minus") {
      0
    } else { -1 } # TODO: calculate both MAGE+ and MAGE- automatically

    n <- 1

    tp_indexes <- indexes

    # Computation:
    # 1. collect all tp ids ??? TODO: what does this do?
    tp_indexes <- sapply(2:nrow(crosses), function(i) {
      # ASSUMES alternating MIN, MAX, MIN, ... (data should be in that arrangement based on selection of indices)
      if (crosses$type[i] == types$REL_MAX) { return }
    })

    # 2. Filter the excursions maybe get ids:

    tp_indexes <- numeric()
    # currently, if the first is below, it moves on to next point (potentially want to accumulate)
    # can we do this in vector form?
    while(n < length(minmax)) {
      height1 <- minmax[n+1] - minmax[n]
      # Redefined variable type, not great
      type_2 <- crosses[n+1, "type"]  ## crosses has 1 more element (from line 163-164) so add 1

      # Check if excursion is above SD. If smaller - go to next excursion.
      if(abs(height1) > standardD) {
        # Check if it was specified whether MAGE+ or MAGE- should be computed.
        # If not specified (-1), determine based on value of height1
        # height1 > 0 means it's nadir to peak
        if(nadir2peak == -1) { # Assigns nadir2peak
          nadir2peak <- ifelse(height1 > 0, 1, 0)
        }

        # Once plus or minus is specified, only look at those excursions that match the type
        if(nadir2peak == type_2) {
          if(n+1 == length(minmax)) { # covers case where one before last
            height2 <- minmax[n+1]-minmax[n]

            if(abs(height2) >= standardD) { # append height2 to height
              tp_indexes <- append(tp_indexes, indexes[n]) # TODO: combine into one?
              tp_indexes <- append(tp_indexes, indexes[n+1])
              heights <- append(heights, abs(height2))
            }
          }
          else {
            x <- 1
            height2 <- 0
            while(!(abs(height2) >= standardD) && n+x+1 <= length(minmax)) {  # checks bounds
              height2 <- minmax[n+x+1]-minmax[n+x]

              if(abs(height2) >= standardD || n+x+1 == length(minmax)-1 || n+x+1 == length(minmax)) {
                # appends height2 to height
                tp_indexes <- append(tp_indexes, indexes[n])
                if(nadir2peak == 1) {
                  heights <- append(heights, abs(minmax[n] - max(minmax[n:(n+x+1)])))
                  tp_indexes <- append(tp_indexes, indexes[n + which.max(minmax[n:(n+x+1)])-1])
                } else {
                  heights <- append(heights, abs(minmax[n]- min(minmax[n:(n+x+1)])))
                  tp_indexes <- append(tp_indexes, indexes[n + which.min(minmax[n:(n+x+1)])-1])
                }
                n <- n+x
              }
              else {
                # this implicitly assumes you have min/max/min/max always alternating - does not work w/ GAPS
                x <- x + 2
              }
            }
          }
        }
      }

      # increment loop variable
      n <- n + 1
    }

    # save intermediate data for plotting
    pframe = parent.frame()
    pframe$all_data = rbind(pframe$all_data, .data)
    pframe$outer_tp_indexes = append(pframe$outer_tp_indexes, tp_indexes)

    # 5. Return MAGE calculation
    if(length(heights) == 0) { # return 0 if no excursions are present
      return(data.frame(start=head(.data$time, 1), end=tail(.data$time, 1), mage=NA, pm=NA, first_excursion=NA))
    }

    return(data.frame(start=head(.data$time, 1), end=tail(.data$time, 1), mage=mean(heights, na.rm=TRUE), pm=NA, first_excursion=TRUE))
  }

  ## 1. Preprocessing
  MA_Short = MA_Long = DELTA_SHORT_LONG = TP = id = .xmin = .xmax = NULL
  rm(list = c("MA_Short", "MA_Long", "DELTA_SHORT_LONG", "TP", ".xmin", ".xmax", "id"))

  data = check_data_columns(data)

  # 1.1 Interpolate over uniform grid
  data_ip <- CGMS2DayByDay(data, dt0 = dt0, inter_gap = inter_gap, tz = tz)
  day_one = lubridate::as_datetime(data_ip$actual_dates[1])
  ndays = length(data_ip$actual_dates)

  # 1.2 Generate grid times by starting from day one and cumulatively summing
  # > replicate dt0 by number of measurements (total minutes/dt0)
  time_ip =  day_one + lubridate::minutes(
    cumsum(
      rep(data_ip$dt0, ndays * 24 * 60 /data_ip$dt0)
    )
  )

  # 1.3 Recalculate short_ma and long_ma because short and long are based on 5 minutes originally
  # > Multiply by 5 to get length in min
  # > Divide by dt0 to get rounded number of measurements that are roughly equal to original short/long ma definition
  short_ma = round(short_ma*5/data_ip$dt0)
  long_ma = round(long_ma*5/data_ip$dt0)

  ## 2. Change to interpolated data (times and glucose)
  # > change data into id, interpolated times, interpolated glucose (t to get rowwise)
  # > drop NA rows before first glucose reading
  # > then drop NA rows after last glucose reading
  # > Label NA glucose as gap (gap = 1)
  data <- data %>%
    dplyr::reframe(id = rep(id[1], length(time_ip)), time = time_ip, gl = as.vector(t(data_ip$gd2d))) %>%
    dplyr::slice(which(!is.na(gl))[1]:dplyr::n()) %>%
    dplyr::slice(1:utils::tail(which(!is.na(.data$gl)), 1)) %>%
    dplyr::mutate(gap = dplyr::if_else(is.na(gl), 1, 0))

  runlen <- rle(data$gap)

  # 3. Sanity Checks
  # 3.1 By definition, long > short MA.
  if (short_ma >= long_ma){
    warning("The short moving average window size should be smaller than the long moving average window size for correct MAGE calculation. Swapping automatically.")
    temp = short_ma
    short_ma = long_ma
    long_ma = temp
  }

  # 3.2 Are any gaps > 12 hours?
  # > since runlen counts by measurements, compare to number of measurements corresponding to 12hrs (720 mins)
  # > take ceiling and add 1 to make edge cases less likely to give this message
  if (any(runlen$lengths[runlen$values == 1] > (ceiling(720/data_ip$dt0) + 1))) {
    message(paste0("Gap found in data for subject id: ", data$id[1], ", that exceeds 12 hours."))
  }

  # 4. Time Series Segmentation: split gaps > max_gap into separate segments
  is_qualifying_gap = runlen$values == 1 & (runlen$lengths*data_ip$dt0 > max_gap)

  if (length(is_qualifying_gap) == 1 && is_qualifying_gap[1] == FALSE) {
    # there are no gaps
    dfs = list()
    dfs[[1]] <- data
  }
  else {
    # there are gaps
    end_boundary = cumsum(runlen$lengths)[is_qualifying_gap]
    start_boundary = end_boundary - runlen$lengths[is_qualifying_gap] + 1 # add 1 b/c both sides inclusive

    dfs = list()

    for (i in seq_along(end_boundary)) {
      curr_gap_start = start_boundary[i]
      curr_gap_end = end_boundary[i]

      if (i == 1) {
        # add 1st data sequence
        if (curr_gap_start > 1) {
          dfs[[length(dfs)+1]] <- data[1:(curr_gap_start - 1), ]
        }
      }

      dfs[[length(dfs) + 1]] <- data[curr_gap_start:curr_gap_end, ]

      if (curr_gap_end < nrow(data)) {
        data_start = curr_gap_end + 1
        data_end = ifelse(i == length(end_boundary), nrow(data), start_boundary[i+1] - 1)

        dfs[[length(dfs) + 1]] <- data[data_start:data_end, ]
      }
    }
  }

  # 5. Calculate MAGE on each identified segment
  all_tp_indexes = c()
  all_data = c()
  return_val = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("start", "end", "mage", "pm", "first_excursion"))

  for (e in dfs) {
      return_val = rbind(return_val, mage_atomic(e))
  }

  # 6. Plotting
  if(plot) {
    # 6.1 Label 'Peaks' and 'Nadirs'
    data <- all_data %>%
      dplyr::mutate(TP = dplyr::case_when(dplyr::row_number() %in% outer_tp_indexes[seq(to=length(outer_tp_indexes), by=2)] ~ ifelse(TRUE,"Peak","Nadir"), # TODO: assumes we alternate Peak/Nadir which does not work w/ gaps
                                          dplyr::row_number() %in% outer_tp_indexes[1+seq(to=length(outer_tp_indexes), by=2)] ~ ifelse(TRUE,"Nadir","Peak")))

    # 6.2 Set a default Title
    title <- if(is.na(title)) {
      paste("Glucose Trace - Subject ", data$id[1])
    } else { title }

    # 6.3 Label Gaps in Data
    interval <- data_ip$dt0

    # filter out gl NAs to enable correct gap identification
    data <- data[complete.cases(data$gl), ]

    # Find the start and end of each gap and merge/sort the two (Must do separately to solve the problem of "back to back" gaps not having correct start & end time)
    .gap_start <- data %>%
      dplyr::filter(abs(difftime(time, dplyr::lead(time), units = "min")) > 2*interval)

    .gap_end <- data %>%
      dplyr::filter(difftime(time, dplyr::lag(time), units = "min") > interval*2)

    .gaps <- rbind(.gap_start, .gap_end) %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(time = dplyr::if_else(dplyr::row_number() %% 2 == 1, time+interval/2, time-interval/2)) # Offset the gap time slightly so not covering peaks/nadirs in some edge cases

    .gaps <- if(nrow(.gaps) %% 2 == 1) { .gaps[1:(nrow(.gaps)-1), ] } else { .gaps } # make the df even

    .gaps <- data.frame(.xmin = .gaps$time[c(TRUE, FALSE)],
                        .xmax = .gaps$time[c(FALSE, TRUE)])

    # extraneous for ggplot; need for plotly
    .ymin <- min(data$gl)
    .ymax <- max(data$gl)

    # 6.4 Generate ggplot
    colors <- c("Short MA" = "#009E73", "Long MA" = "#D55E00","Nadir" = "blue", "Peak"="red")
    gap_colors <- c("Gap"="purple")
    .p <- ggplot2::ggplot(data, ggplot2::aes(x=time, y=gl)) +
      ggplot2::ggtitle(title) +
      ggplot2::geom_point() +
      ggplot2::geom_point(data = subset(data, data$TP != ""), ggplot2::aes(color = TP), fill='white', size=2) +
      ggplot2::geom_rect(data=.gaps, ggplot2::aes(
        xmin=.xmin,
        xmax=.xmax,
        ymin=.ymin,
        ymax=.ymax, fill = 'Gap'),
        alpha=0.2, inherit.aes = FALSE, show.legend = T, na.rm = TRUE) +
      ggplot2::theme(
        legend.key = element_rect(fill='white'),
        legend.title = ggplot2::element_blank(),
      ) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_fill_manual(values=gap_colors) +
      ggplot2::labs(x=ifelse(!is.na(xlab), xlab, "Time"), y=ifelse(!is.na(ylab), ylab, 'Glucose Level'))

    if(show_ma == TRUE) {
      .p <- .p + ggplot2::geom_line(ggplot2::aes(y = MA_Short, group = 1, color="Short MA")) + #Exclude for now because ggplot becomes too crowded
        ggplot2::geom_line(ggplot2::aes(y = MA_Long, group = 2, color="Long MA"))
    }

    # 4d. Return plot
    # return(.p)
    return(plotly::ggplotly(.p))
  }

  else {
    return_type = match.arg(return_type, c('num', 'df'))
    return_ = match.arg(return_, c('auto', 'plus', 'minus'))

    if (return_type == 'df') {
      return(return_val)
    }

    # filter by various options
    if (return_ == 'plus') {
      res = return_val %>% dplyr::filter(pm = 'PLUS')
    } else if (return_ == 'minus') {
      res = return_val %>% dplyr::filter(pm == 'MINUS')
    } else {
      res = return_val %>% dplyr::filter(first_excursion)
    }

    if (nrow(res) == 0) {
      return(NA)
    }

    res = res %>%
      dplyr::mutate(hours = end - start) %>%
      dplyr::mutate(weight = as.numeric(hours/as.numeric(sum(hours)))) %>% # weight each segment's mage value contribution by segment length
      dplyr::summarize(sum(mage*weight))

    return(as.numeric(res))
  }
}

