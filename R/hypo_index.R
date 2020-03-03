#' Calculate Hypoglycemia Index
#'
#' @description
#' The function hypo_index produces Hypoglycemia index values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' hypo_index(data, lower = 70)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of Hypoglycemic Index.
#'
#' @param lower Lower bound used for hypoglycemia cutoff. Default is 70
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' Hypoglycemia Index is calculated by \eqn{n/30 * \sum [hypoBG_j ^{2}]}
#' Where n is the total number of Blood Glucose measurements and \eqn{hypoBG_j}
#' is the jth Blood Glucose measurement below the hypoglycemia cutoff.
#'
#' Wrapping as.numeric() around the hypo_index call on a dataset with
#' a single subject will return a numeric value corresponding to the
#' Hypoglycemia Index value. This will not work for datasets with multiple subjects.
#'
#' @return
#'
#' @export
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11 Suppl 1},
#' S55-67. \doi{10.1089/dia.2008.0132}.
#'
#' @examples
#' data(example_data_1_subject)
#' res = hypo_index(example_data_1_subject)
#' hp1 = res$hypo_index
#' res = hypo_index(example_data_1_subject$gl)
#' hp2 = res$hypo_index
#' stopifnot(isTRUE(all.equal(hp1, hp2)))
#' hypo_index(example_data_1_subject, lower = 60)
#'
#' data(example_data_5_subject)
#' hypo_index(example_data_5_subject)
#' hypo_index(example_data_5_subject, lower = 80)
#'

hypo_index <- function(data, lower = 70){
  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      hypo_index = sum( (lower - gl[gl < lower]) ^ 2, na.rm = TRUE) /
        (sum(!is.na(gl)) * 30)
    )
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}
