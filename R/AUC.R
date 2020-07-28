#' Calculate Area Under Curve AUC
#'
#' @description
#' The function auc produces hourly average AUC for each subject.
#'
#' @usage
#' auc(data, tz="")
#'
#' @param data DataFrame object with column names "id", "time", and "gl", or numeric vector of glucose values.
#' @param tz String value of time zone
#'
#' @return
#' If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding hourly average AUC value is returned.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for hourly average AUC values is returned. NA glucose values are
#' omitted from the calculation of the AUC.
#'
#' AUC is calculated using the formula: (dt0/60) * ((gl[2:length(gl)] + gl[1:(length(gl)-1)])/2),
#' where dt0/60 is the frequency of the cgm measurements in hours and gl are the glucose values.
#'
#' @references
#' Danne et al. (2017) International Consensus on Use of Continuous Glucose Monitoring,
#' \emph{Diabetes care} \strong{29} .1631-1640,
#' \doi{10.2337/dc06-1085}
#'
#' @examples
#' data(example_data_1_subject)
#' auc(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' auc(example_data_5_subject)



# Note about the iglu interpolation function CGMS2DayByDay:
# This function returns a list with three items:
# interpolated glucose values, the days of data, and the frequency of CGM measurements
# To access each respectively we use [[]]

auc <- function (data, tz = "") {

  # this is a helper function that runs on a single person's data
  auc_single <- function(data) {
    # dt0 is the frequency of the cgm measurements, typically 5 min but could be different
    dt0 = CGMS2DayByDay(data, tz = tz)[[3]]
    # this whole part gets our desired output from the data
    out = data %>%
      # this part tidies up the data into 2 columns: day and gl
      dplyr::summarise(
        # from the interpolation, we get the days of data
        day = rep(CGMS2DayByDay(data.frame(id, gl, time))[[2]],
                  # we replicate each day by the number of measurements for that day
                  1440/dt0),
        # the interpolated glucose is input as a vector
        gl = as.vector(t(CGMS2DayByDay(data.frame(
          id, time, gl))[[1]]))
      ) %>%
      # this part finds the area under the curve
      # first we group by day
      dplyr::group_by(day) %>%
      dplyr::summarise(
        # this returns the area measurements for each trapezoid
        # AUC is calculated using the formula:
        # (dt0/60) * ((gl[2:length(gl)] + gl[1:(length(gl)-1)])/2)
        each_area = (dt0/60) * ((gl[2:length(gl)] +
                                   gl[1:(length(gl)-1)])/2)
      ) %>%
      # here we summarise to daily area, then hourly average
      dplyr::summarise(daily_area = sum(each_area, na.rm = TRUE),
                # we need the actual hours collected because some data has NA gaps
                hours = dt0/60 * length(na.omit(each_area)),
                # then we find the hourly average for each day
                hourly_avg = daily_area/hours)
    return(out)
  }

  # here we split the data by id then apply the helper function
  out = data %>%
    dplyr::group_by(id) %>%
    # the overall average for each subject is the mean of their daily hourly avg
    dplyr::summarise(hourly_auc = mean(
      auc_single(data.frame(id, gl, time))$hourly_avg))
  # in the end, we return an overall hourly average for each subject
  return(out)
}


