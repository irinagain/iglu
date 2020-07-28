#' Calculate Coefficient of Variation subtypes
#'
#' @description
#' The function cv_measures produces CV subtype values in a tibble object.
#'
#' @usage
#' cv_measures(data, dt0 = NULL, inter_gap = 45, tz = "" )
#'
#' @inheritParams CGMS2DayByDay
#' @inheritParams conga
#'
#'
#' @return When a data.frame object is passed, then a tibble object with
#' three columns: subject id and corresponding CV subtype values is returned.
#'
#'
#' @export
#'
#' @details
#'
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for each cv subtype values is returned.
#'
#' Missing values will be linearly interpolated when close enough to non-missing values.
#'
#' \enumerate{
#' \item Mean:
#'
#' Calculated by first taking the coefficient of variation of each day's glucose measurements,
#' then taking the mean of all the coefficient of variation. That is, for x
#' days we compute cv_1 ... cv_x daily coefficient of variations and calculate
#' \eqn{1/x * \sum [(cv_i)]}
#'
#' \item Sd:
#'
#' Calculated by first taking the coefficient of variation of each day's glucose measurements,
#' then taking the standard deviation of all the coefficient of variations. That is, for d
#' days we compute cv_1 ... cv_d daily coefficient of variations and calculate SD([cv_1, cv_2, ... cv_d])
#' }
#'
#' @references
#' Umpierrez, et.al. (2018) Glycemic Variability: How to Measure and Its Clinical
#' Implication for Type 2 Diabetes
#' \emph{The American Journal of Medical Sciences} \strong{356} .518-527,
#' \doi{10.1016/j.amjms.2018.09.010}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' cv_measures(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' cv_measures(example_data_5_subject)
#'



cv_measures <- function(data, dt0 = NULL, inter_gap = 45, tz = "" ){


  abs_diff_mean = gl = id = NULL
  rm(list = c("gl", "id", "abs_diff_mean"))
  data=check_data_columns(data)
  is_vector = attr(data, "is_vector")

  cv<- function(data, na.rm = FALSE ){
    return((sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE))*100)
    }

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      CVmean=CGMS2DayByDay(data.frame(id,time,gl),dt0 = dt0, tz = tz, inter_gap = inter_gap)$gd2d %>%
        apply( 1, cv, na.rm = TRUE) %>%
        mean( na.rm = TRUE),

      CVsd=CGMS2DayByDay(data.frame(id,time,gl),dt0 = dt0, tz = tz, inter_gap = inter_gap)$gd2d %>%
        apply( 1, cv, na.rm = TRUE) %>%
        sd( na.rm = TRUE))
  if (is_vector) {
    out$id = NULL
  }
  return(out)

}
