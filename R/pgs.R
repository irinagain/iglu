#' Calculate Personal Glycemic State (PGS)
#'
#' @description
#' The function mad produces PGS values in a tibble object.
#'
#' @usage
#' pgs(data, dur_length = 20, end_length = 30)
#'
#' @inheritParams episode_calculation
#' @param dur_length Numeric value specifying the minimum duration in minutes to
#' be considered an episode. Note dur_length should be a multiple of the data recording
#' interval otherwise the function will round up to the nearest multiple.
#' Default is 20 minutes to match the original PGS definition.
#' @param end_length Numeric value specifying the minimum duration in minutes of
#' improved glycemia for an episode to end.
#' Default is 30 minutes to match original PGS definition.
#'
#' @return A tibble object with two columns:
#' subject id and corresponding PGS value.
#'
#' @export
#'
#' @details
#'
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for PGS values is returned. NA glucose values are
#' omitted from the calculation. The formula for PGS is as follows,
#' where GVP = glucose variability percentage, MG = mean glucose,
#' PTIR = percent time in range, and N54, N70 are the number of hypoglycemic episodes
#' per week in the ranges <54 mg/dL and 54 to <70 mg/dL level respectively.
#'
#' \deqn{
#' PGS = f(GVP) + g(MG) + h(PTIR) + j(N54, N70)
#' }
#'
#' The component functions are listed below.
#'
#' \deqn{
#' \newline
#' f(GVP) = 1 + \frac{9}{1+\exp(-0.049(GVP - 65.47))}
#' \newline
#' g(MG) = 1 + 9(\frac{1}{1+\exp(0.1139(MG - 72.08))} + \frac{1}{1+\exp(-0.09195(MG - 157.57))})
#' \newline
#' h(PTIR) = 1+\frac{9}{1+\exp(0.0833(PTIR - 55.04))}
#' \newline
#' j(N54, N70) = a(N54) + b(N70)
#' \newline
#' a(N54) = 0.5+4.5(1-\exp(-0.91093N54)
#' }
#'
#' and b(N70) is defined such that b(N70) = \eqn{0.5714N70 + 0.625} if N70 <= 7.65, and b(N70) = 5 otherwise.
#'
#'
#' Note that the duration thresholds for episodes
#' are NOT the same as the episode_calculation defaults. The defaults chosen for
#' PGS are those that match the original PGS paper definition, while the
#' episode_calculation defaults match the consensus.
#'
#' @seealso episode_calculation()
#'
#' @author Elizabeth Chun
#'
#' @references
#' Hirsch et al. (2017): A Simple Composite Metric for the Assessment of Glycemic
#' Status from Continuous Glucose Monitoring Data: Implications for Clinical Practice
#' and the Artificial Pancreas
#' \emph{Diabetes Technol Ther} \strong{19(S3)} .S38-S48,
#' \doi{10.1089/dia.2017.0080}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' pgs(example_data_1_subject)
#'
#'

pgs = function(data, dur_length = 20, end_length = 30) {


  time = gl = id = NULL
  rm(list = c("time", "gl", "id"))

  data = check_data_columns(data)

  pgs_single = function(data) {

    f_gvp = 1 + (9/(1+exp(-0.049*(gvp(data)$GVP - 65.47))))
    f_ptir = 1 + (9/(1+exp(0.0833*(in_range_percent(data)$in_range_70_180 - 55.04))))
    f_mg = 1 + 9*((1/(1+exp(0.1139*(mean_glu(data)$mean - 72.08)))) +
                    (1/(1+exp(-0.09195*(mean_glu(data)$mean - 157.57)))))

    eps = episode_calculation(data, lv1_hypo = 70, lv2_hypo = 54,
                              dur_length = dur_length, end_length = end_length)
    f_h54 = 0.5 + 4.5*(1-exp(-0.81093*eps$avg_ep_per_day[2]*7))
    n70 = eps$avg_ep_per_day[6]*7 # use lv1 exclusive, not lv1 super set
    f_h70 = ifelse(n70 <= 7.65, 0.5714*n70 + 0.625, 5)

    out = f_gvp + f_ptir + f_mg + f_h54 + f_h70

    return(out)
  }

  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(PGS = pgs_single(data.frame(id,time,gl)))

  return(out)
}
