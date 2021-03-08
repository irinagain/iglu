#' Calculates Mean Amplitude of Glycemic Excursions (see "mage")
#'
#' @description This function is an internal function used by "mage". The function will calculate the Mean Amplitude of Glycemic Excursions (MAGE) on \strong{all} the values of the inputted data set. To calculate separate MAGE values for a group of subjects, use the "mage" function.
#'
#' @author Nathaniel Fernandes
#' @details See "mage".
#'
#' @param data Data Frame object with column names "id", "time", and "gl" OR numeric vector of glucose values (plot won't work with vector)
#' @param short_ma Integer for period length of the short moving average. Must be positive and less than "long_ma". (Recommended <15)
#' @param long_ma Integer for period length for the long moving average. (Recommended >20)
#' @param plot Boolean. Returns ggplot if TRUE.
#' @param interval Integer for time interval in minutes between glucose readings. Function will auto-magically determine the interval if not specified. (Only used to calculate the gaps shown on the ggplot)
#' @param dateformat POSIXct time format for time of glucose readings. Highly recommended to set if glucose times are in a different format.
#' @param title Title for the ggplot. Defaults to "Glucose Trace - Subject [ID]"
#' @param xlab Label for x-axis of ggplot. Defaults to "Time"
#' @param ylab Label for y-axis of ggplot. Defaults to "Glucose Level"
#'
#' @return The numeric MAGE value for the inputted glucose values or a ggplot if \code{plot = TRUE}
#'
#' @export
#'
#' @examples
#' data(example_data_5_subject)
#' mage_cross_single(
#'    example_data_5_subject,
#'    short_ma = 4,
#'    long_ma = 24)
#'
#' mage_cross_single(
#'    example_data_5_subject,
#'    dateformat="%m-%d-%Y %H:%M:%S")
#'
#' mage_cross_single(
#'    example_data_5_subject,
#'    plot=TRUE,
#'    interval=15,
#'    title="Patient X",
#'    xlab="Time",
#'    ylab="Glucose Level (mg/dL)")


mage_cross_single <- function(data, short_ma = 5, long_ma = 23, plot = FALSE, interval=NA, dateformat="%Y-%m-%d %H:%M:%S", title = NA, xlab=NA,ylab = NA) {

  ## 1. Preprocessing
  # 1a. Clean up Global Environment
  MA_Short = MA_Long = DIFF = TP = .xmin = .xmax = NULL
  rm(list = c("MA_Short", "MA_Long", "DIFF", "TP", ".xmin", ".xmax"))

  # 1b. Sanitize the input data
  data = check_data_columns(data)

  ## 2. Process the Data
  # 2a. Calculate the moving average values
  .data <- data %>%
    dplyr::mutate(time = as.POSIXct(time, format = dateformat),
                  MA_Short = zoo::rollmean(gl, short_ma, align = 'right', fill = NA),
                  MA_Long  = zoo::rollmean(gl, long_ma,  align = 'right', fill = NA),
                  MA_Short = replace(MA_Short, 1:short_ma, MA_Short[short_ma]),
                  MA_Long  = replace(MA_Long, 1:long_ma, MA_Long[long_ma]),
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
  if(plot) {

    # 4a. Label 'Peaks' and 'Nadirs'
    .data <- .data %>%
      dplyr::mutate(TP = dplyr::case_when(dplyr::row_number() %in% tp_indexes[seq(to=length(tp_indexes), by=2)] ~ ifelse(nadir2peak==0,"Peak","Nadir"),
                                          dplyr::row_number() %in% tp_indexes[1+seq(to=length(tp_indexes), by=2)] ~ ifelse(nadir2peak==0,"Nadir","Peak")))

    #Set a default Title
    title <- if(is.na(title)) {
      paste("Glucose Trace - Subject ", .data$id[1])
    } else { title }
    # 4b. Label Gaps in Data
    # Automagically calculate interval of glucose monitor if unspecified
    interval <- if(is.na(interval)) {
      diff <- as.numeric(nrow(.data) - 1)

      for( i in 2:nrow(.data)) {
        diff[i-1] = .data$time[i] - .data$time[i-1]
      }
      median(diff, na.rm = T)
    } else { interval }

    # Find the start and end of each gap and merge/sort the two (Must do separately to solve the problem of "back to back" gaps not having correct start & end time)
    .gap_start <- .data %>%
      dplyr::filter(abs(difftime(time, dplyr::lead(time), units = "min")) > 2*interval)

    .gap_end <- .data %>%
      dplyr::filter(difftime(time, dplyr::lag(time), units = "min") > interval*2)

    .gaps <- rbind(.gap_start, .gap_end) %>%
      dplyr::arrange(time) %>%
      dplyr::mutate(time = dplyr::if_else(dplyr::row_number() %% 2 == 1, time+interval/2, time-interval/2)) # Offset the gap time slightly so not covering peaks/nadirs in some edge cases

    .gaps <- if(nrow(.gaps) %% 2 == 1) { .gaps[1:(nrow(.gaps)-1), ] } else { .gaps } # make the df even

    .gaps <- data.frame(.xmin = .gaps$time[c(TRUE, FALSE)],
                        .xmax = .gaps$time[c(FALSE, TRUE)])

  #  .ymin <- min(.data$gl) #extraneous for ggplot; need for plotly
  #  .ymax <- max(.data$gl)

    # 4c. Generate ggplot
    colors <- c("Short MA" = "#D55E00", "Long MA" = "#009E73","Nadir" = "blue", "Peak"="red", "Gap"="purple")

    .p <- ggplot2::ggplot(.data, ggplot2::aes(x=time, y=gl)) +
      ggplot2::ggtitle(title) +
      ggplot2::geom_point() +
      ggplot2::geom_point(data = subset(.data, .data$TP != ""), ggplot2::aes(color = TP), fill='white', size=2) +
     # ggplot2::geom_line(ggplot2::aes(y = MA_Short, group = 1, color="Short MA")) + #Exclude for now because ggplot becomes too crowded
     # ggplot2::geom_line(ggplot2::aes(y = MA_Long, group = 2, color="Long MA")) +
      ggplot2::geom_rect(data=.gaps, ggplot2::aes(
        xmin=.xmin,
        xmax=.xmax,
        ymin=-Inf,
        ymax=Inf, fill = 'Gap'),
        alpha=0.2, inherit.aes = FALSE, show.legend = T) +
      ggplot2::theme(
        legend.key = element_rect(fill='white'),
        legend.title = ggplot2::element_blank(),
        ) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_fill_manual(values=colors) +
      ggplot2::labs(x=ifelse(!is.na(xlab), xlab, "Time"), y=ifelse(!is.na(ylab), ylab, 'Glucose Level'))

    # 4d. Return plot
    return(.p)
    #return(plotly::ggplotly(.p))
  }

  # 5. Return MAGE calculation
  if(length(heights) == 0) { # return 0 if no excursions are present
    return(0)
  }
  mean(heights)
}
