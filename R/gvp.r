#' Calculate Glucose Variability Percentage (GVP)
#'
#' @description
#' The function mad produces GVP values in a tibble object.
#'
#' @usage
#' gvp(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl"
#'
#' @return A tibble object with two columns:
#' subject id and corresponding GVP value.
#'
#' @export
#'
#' @details
#'
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for GVP values is returned. NA glucose values are
#' omitted from the calculation of the GVP.
#'
#' GVP is calculated by dividing the total length of the line of the glucose trace
#' by the length of a perfectly flat trace. The formula for this is
#' \eqn{sqrt(diff^2+dt0^2)/(n*dt0)}, where diff is the change in
#' Blood Glucose measurements from one reading to the next,
#' dt0 is the time gap between measurements and n is the number of glucose readings
#'
#' @author David Buchanan, Mary Martin
#'
#' @references
#' Peyser et al. (2017) Glycemic Variability Percentage: A Novel Method for Assessing
#' Glycemic Variability from Continuous Glucose Monitor Data.
#' \emph{Diabetes Technol Ther} \strong{20}(1):6–16,
#' \doi{10.1089/dia.2017.0187}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' gvp(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' gvp(example_data_5_subject)
#'

gvp = function(data) {


  time = gl = id = NULL
  rm(list = c("time", "gl", "id"))

  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")


  gvp_single = function(subj) {
    data_ip = iglu::CGMS2DayByDay(subj)
    daybyday = as.vector(t(data_ip[[1]]))
    reading_gap = data_ip[[3]]

    diffvec = diff(daybyday, na.rm = T)
    added_length = sqrt(reading_gap^2+diffvec^2)
    base_length = length(na.omit(diffvec))*reading_gap

    return(sum(added_length, na.rm = T)/sum(base_length, na.rm = T))
  }

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(GVP = gvp_single(data.frame(id,time,gl)))

  return(out)
}
