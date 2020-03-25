#' Calculate the M-value
#'
#' @description Calculates the M-value of Schlichtkrull et al. (1965) for
#' each subject in the data, where the M-value is the mean of the logarithmic
#' transformation of the deviation from a reference value.
#'
#' @usage
#' mvalue(data)
#'
#' @inheritParams mean_glu
#' @param r A reference value corresponding to basal glycemia in normal
#'subjects; default is 90 mg/dL.
#'
#' @return A data.frame with two columns: subject id and corresponding M value.
#'
#' @export
#'
#' @references
#' Schlichtkrull J, Munck O, Jersild M. (1965) The M-value, an index of
#' blood-sugar control in diabetics. \emph{Acta Medica Scandinavica}
#' \strong{177}, 95-102.
#' \doi{https://doi.org/10.1111/j.0954-6820.1965.tb01810.x}.
#'
#' @examples
#' data(example_data_5_subject)
#'
#' mvalue(example_data_5_subject)
#' mvalue(example_data_5_subject, r = 100)
#'
mvalue <- function(data, r = 90){
  x = id = NULL
  rm(list = c("id", "x"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")
  out = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      Mvalue = mean(1000 * abs(log10(gl / r)) ^ 3, na.rm = T)
    )

  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
