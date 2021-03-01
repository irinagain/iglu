#' Calculate Mean Amplitude of Glycemic Excursions
#'
#' @description UPDATED to version 2. The function calculates MAGE values and can optionally return a plot of the glucose trace. Version 1 is also accessible for backwards compatibility.
#'
#' @usage
#' mage(data)
#' mage(data, .plot = TRUE)
#' mage(data, short_MA = 7, long_MA = 20)
#' mage(data, version = 'v1')
#'
#' @param data Data Frame object with column names "id", "time", and "gl" OR numeric vector of glucose values (plot won't work with vector).
#'
#' @param version Optional. Either 'v2' or 'v1'. Chooses which version of the MAGE algorithm to use. Version 2 is default and highly recommended.
#'
#' @param ... Optional arguments to pass to the MAGE Functions
#' \itemize{
#'   \item{dateformat: POSIXct format for time of glucose reading. Default: YYYY-mm-dd HH:MM:SS. Highly recommended to set if glucose times are of a different format.}
#'   \item{short_MA: Integer for period length of the short moving average. Must be positive and less than "long_MA". Default: 5. (Recommended <15).}
#'   \item{long_MA: Integer for period length for the long moving average. Default: 23. (Recommended >20)}
#'  \item{.plot: Boolean. Returns ggplot if TRUE. Default: FALSE.}
#'  \item{.interval: Integer for time interval in minutes between glucose readings. Algorithm will auto-magically determine the interval if not specified. Default: NA (Only used to calculate the gaps shown on the ggplot)}
#'  \item{.title: Title for the ggplot. Default: "Glucose Trace"}
#'  \item{sd_multiplier: DEPRECATED. A numeric value that can change the sd value used to determine size of glycemic excursions used in the calculation.}
#' }
#'
#' @return Version 2: The calculated MAGE value for the glucose values or a ggplot if plot = TRUE. Version 1: If a data.frame object is passed, then a tibble object with two columns - subject id and corresponding MAGE value is returned. If a vector of glucose values is passed, then a tibble object with just the MAGE value is returned. as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @details The function computationally emulates the manual method for calculating the mean amplitude of glycemic excursions (MAGE) first suggested in Mean Amplitude of Glycemic Excursions, a Measure of Diabetic Instability, (Service, 1970). The proposed method uses the crosses of a short and long moving average to identify intervals where a peak or nadir may exist. Then, the height from one peak/nadir to the next nadir/peak is calculated from the *original* glucose values.
#'
#'
#' @references
#' Service et al. (1970) Mean amplitude of glycemic excursions, a measure of diabetic instability
#' \emph{Diabetes}  \strong{19} .644-655,
#' \doi{10.2337/diab.19.9.644}.
#'
#' @examples
#' mage(data)
#' mage(data, dateformat = "%m-%d-%Y %H:%M:%S")
#' mage(data, short_MA = 4, long_MA = 24)
#' mage(data, .plot = TRUE, .interval = 15, .title="Glucose Trace for Patient X")
#'
#' DEPRECATED.
#' mage(data, version = 'v1')
#' mage(example_data_1_subject, version = 'v1', sd_multiplier = 2)

mage <- function(data, version = c('v2', 'v1'), ...) {

  # Match version
  version = match.arg(version)

  if(version == 'v1') {
    warning("You are using Version 1 of the iglu mage algorithm. This function is deprecated and will return SIGNIFICANT errors. Please use Version 2")
    return(mage_old(data, ...))
  }

  return(mage_new(data, ...))
}

mage_new <- function(data, ...) {
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
#
  if(exists('.plot') && .plot == TRUE) {
    plot_list = data %>%
      dplyr::filter(!is.na(gl)) %>%
      dplyr::group_by(id) %>%
      dplyr::do(MAGE = mage_single(., ...))

    return(plot_list)
  }

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::do(data.frame(MAGE = mage_single(., ...)))


  if (is_vector) {
    out$id = NULL
  }

  return(out)
}

mage_single <- function(data, short_MA = 5, long_MA = 23, .plot = FALSE, .interval=NA,
                     dateformat="%Y-%m-%d %H:%M:%S", .title = "Glucose Trace") {

  ## 1. Preprocessing
  # 1a. Clean up Global Environment
  MA_Short = MA_Long = DIFF = TP = .xmin = .xmax = NULL
  rm(list = c("MA_Short", "MA_Long", "DIFF", "TP", ".xmin", ".xmax"))

  # 1b. Sanitize the input data
  #data = check_data_columns(data)

  ## 2. Process the Data
  # 2a. Calculate the moving average values
  .data <- data %>%
    dplyr::mutate(time = as.POSIXct(time, format = dateformat),
                  MA_Short = zoo::rollmean(gl, short_MA, align = 'right', fill = NA),
                  MA_Long  = zoo::rollmean(gl, long_MA,  align = 'right', fill = NA),
                  MA_Short = replace(MA_Short, 1:short_MA, MA_Short[short_MA]),
                  MA_Long  = replace(MA_Long, 1:long_MA, MA_Long[long_MA]),
                  DIFF = MA_Short - MA_Long)

  # 2b. Create a preallocated list of crossing point ids & type
  list_cross <- list("id" = rep.int(NA, 1000), "type" = rep.int(NA, 1000))
  list_cross$id[1] <- 1
  list_cross$type[1] <- ifelse(.data$DIFF[1] > 0, 1, 0)
  count = 1
  for(i in seq_along(.data$DIFF)) {
    if(i >= 2 && !is.na(.data$DIFF[i]) && !is.na(.data$DIFF[i-1])) {
      # crossing point if previous DIFF changes sign or goes to 0
      if(.data$DIFF[i] * .data$DIFF[i-1] < 0 ||
         (.data$DIFF[i] == 0 && .data$DIFF[i-1] != 0)) {
        count <- count + 1
        list_cross$id[count] <- i
        if(.data$DIFF[i] < .data$DIFF[i-1]) {
          list_cross$type[count] = 0
          # 0 means short crossed below so relative minimum
        } else {
          list_cross$type[count] = 1
        }
      }
    }
  }
  # Add last point to capture end variation
  list_cross$id[count+1] <- nrow(.data)
  list_cross$type[count+1] <- ifelse(.data$DIFF[nrow(.data)] > 0, 1, 0)

  # 2c. Filter for non-na values then combine into a table
  list_cross$id <- list_cross$id[!is.na(list_cross$id)]
  list_cross$type <- list_cross$type[!is.na(list_cross$type)]

  crosses <- do.call(cbind, list_cross)

  # 2d. Calculate min and max glucose values from ids and types in crosses
  #     - store indexes for plotting later
  minmax <- numeric(0)
  indexes <- numeric(0)
  for(i in 1:(nrow(crosses)-1)) {
    s1 <- crosses[i,1]    # indexes of crossing points
    s2 <- crosses[i+1,1]

    if(crosses[i, "type"] == 0) {
      minmax <- append(minmax, min(.data$gl[s1:s2]))
      indexes <- append(indexes,which.min(.data$gl[s1:s2])+s1-1) # append index to array
    } else {
      minmax <- append(minmax, max(.data$gl[s1:s2]))
      indexes <- append(indexes, which.max(.data$gl[s1:s2])+s1-1)
    }
  }

  ## 3. Calculate MAGE
  # 3a. Standard deviation
  standardD <- sd(.data$gl, na.rm = TRUE)

  # 3b. Calculate the valid excursion heights
  #     - heights: vector that will contain final heights
  #     - peak2nadir: measure excursion peak to nadir. unassigned (-1), false (0), true (1)
  #     - n: a counter that helps iterate through the turning points
  #     - tp_indexes: vector that will contain tp from valid excursions
  heights <- numeric()
  nadir2peak <- -1
  n <- 1
  tp_indexes <- numeric()

  while(n < length(minmax)) {
    height1 <- minmax[n+1] - minmax[n]
    type <- crosses[n+1, "type"]  ## crosses has 1 more element so add 1

    if(abs(height1) > standardD) {
      if(nadir2peak == -1) { # Assigns nadir2peak
        nadir2peak <- ifelse(height1 > 0, 1, 0)
      }

      if(nadir2peak == type) {
        if(n+1 == length(minmax)) { # covers case where one before last
          height2 <- minmax[n+1]-minmax[n]

          if(abs(height2) >= standardD) { # append height2 to height
            tp_indexes <- append(tp_indexes, indexes[n])
            if(nadir2peak == 1) {
              heights <- append(heights, abs(height2))
              tp_indexes <- append(tp_indexes, indexes[n+1])
            } else {
              heights <- append(heights, abs(height2))
              tp_indexes <- append(tp_indexes, indexes[n+1])
            }
          }
        }

        else {
          x <- 1
          height2 <- 0
          while(!(abs(height2) >= standardD) && n+x+1 <= length(minmax)) {  # checks bounds
            height2 <- minmax[n+x+1]-minmax[n+x]

            if(abs(height2) >= standardD || n+x+1 == length(minmax)-1 || n+x+1 == length(minmax)) { # appends height2 to height
              tp_indexes <- append(tp_indexes, indexes[n])
              if(nadir2peak == 1) {
                heights <- append(heights, abs(minmax[n]- max(minmax[n:(n+x+1)])))
                tp_indexes <- append(tp_indexes, indexes[n + which.max(minmax[n:(n+x+1)])-1])
              } else {
                heights <- append(heights, abs(minmax[n]- min(minmax[n:(n+x+1)])))
                tp_indexes <- append(tp_indexes, indexes[n + which.min(minmax[n:(n+x+1)])-1])
              }
              n <- n+x
            }
            else {
              x <- x + 2
            }
          }
        }
      }
    }

    # increment loop variable
    n <- n + 1
  }

  ## 4. Generate Plot of Data (if specified)
  if(.plot) {

    # 4a. Label 'Peaks' and 'Nadirs'
    .data <- .data %>%
      dplyr::mutate(TP = dplyr::case_when(row_number() %in% tp_indexes[seq(to=length(tp_indexes), by=2)] ~ ifelse(nadir2peak==0,"Peak","Nadir"),
                                          row_number() %in% tp_indexes[1+seq(to=length(tp_indexes), by=2)] ~ ifelse(nadir2peak==0,"Nadir","Peak")))
    #View(.data)
    # 4b. Label Gaps in Data
    # Automagically calculate interval of glucose monitor if unspecified
    .interval <- if(is.na(.interval)) {
      diff <- as.numeric(nrow(.data) - 1)

      for( i in 2:nrow(.data)) {
        diff[i-1] = .data$time[i] - .data$time[i-1]
      }
      median(diff, na.rm = T)
    } else { .interval }

    # # Label Data
    .gaps <- .data %>%
        dplyr::filter(difftime(time, dplyr::lag(time), units = "min") >= .interval*2 |
               abs(difftime(time, dplyr::lead(time), units = "min")) >= 2*.interval)
    # View(.gaps)
    .gaps <- if(nrow(.gaps) %% 2 == 1) { .gaps[1:(nrow(.gaps)-1), ] } else { .gaps } # make the df even

    .gaps <- data.frame(.xmin = .gaps$time[c(TRUE, FALSE)],
                        .xmax = .gaps$time[c(FALSE, TRUE)])
    .ymin <- min(.data$gl)
    .ymax <- max(.data$gl)

    # 4c. Generate ggplot
    colors <- c("Short MA" = "#D55E00", "Long MA" = "#009E73","Nadir" = "blue", "Peak"="red", "Gap"="purple")

    .p <- ggplot2::ggplot(.data, ggplot2::aes(x=time, y=gl)) +
      ggplot2::ggtitle(.title) +
      ggplot2::geom_point() +
      ggplot2::geom_point(data = subset(.data, .data$TP != ""), ggplot2::aes(color = TP), size=2) +
      ggplot2::geom_line(ggplot2::aes(y = MA_Short, group = 1, color="Short MA")) +
      ggplot2::geom_line(ggplot2::aes(y = MA_Long, group = 2, color="Long MA")) +
      ggplot2::geom_rect(data=.gaps, ggplot2::aes(
        xmin=.xmin,
        xmax=.xmax,
        ymin=.ymin,
        ymax=.ymax,
        color = "Gaps"),
        fill="purple",
        alpha=0.2, inherit.aes = FALSE) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::scale_color_manual(values = colors)

    # 4d. Return plot
    return(plotly::ggplotly(.p))
  }

  # 5. Return MAGE calculation
  if(length(heights) == 0) { # return 0 if no excursions are present
    return(0)
  }
  mean(heights)
}

mage_old <- function(data, sd_multiplier = 1){

  abs_diff_mean = gl = id = NULL
  rm(list = c("gl", "id", "abs_diff_mean"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(abs_diff_mean = abs(gl - mean(gl, na.rm = TRUE))) %>%
    dplyr::summarise(
      MAGE = mean(
        abs_diff_mean[abs_diff_mean > (sd_multiplier * sd(gl, na.rm = TRUE))],
        na.rm = TRUE)
    )
  #if (is_vector) {
  #  out$id = NULL
  #}
  return(out)

}

