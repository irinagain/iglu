#' Calculate the M-value
#'
#' @description Calculates the M-value of Schlichtkrull et al. (1965) for
#' each subject in the data, where the M-value is the mean of the logarithmic
#' transformation of the deviation from a reference value. Produces a tibble
#' object with subject id and M-values.
#'
#' @usage
#' m_value(data, r = 90)
#'
#' @inheritParams mean_glu
#' @param r A reference value corresponding to basal glycemia in normal
#' subjects; default is 90 mg/dL.
#'
#' @return If a data.frame object is passed, then a tibble object with
#' two columns: subject id and corresponding M-value is returned. If a vector of glucose
#' values is passed, then a tibble object with just the M-value is returned.
#' as.numeric() can be wrapped around the latter to output just a numeric value.
#'
#' @export
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the M-values is returned. NA glucose values are
#' omitted from the calculation of the M-value.
#'
#' M-value is computed by averaging the transformed glucose values, where each
#' transformed value is equal to \eqn{|10 * log_10(glucose/r)|^3}, where r is the specified reference value.
#'
#' @references
#' Schlichtkrull J, Munck O, Jersild M. (1965) The M-value, an index of
#' blood-sugar control in diabetics.
#' \emph{Acta Medica Scandinavica} \strong{177} .95-102.
#' \doi{10.1111/j.0954-6820.1965.tb01810.x}.
#'
#' @examples
#' data(example_data_5_subject)
#'
#' m_value(example_data_5_subject)
#' m_value(example_data_5_subject, r = 100)
#'
m_value <- function(data, r = 90){
  x = id = NULL
  rm(list = c("id", "x"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      M_value = mean(1000 * abs(log10(gl / r)) ^ 3, na.rm = TRUE)
    )

  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
