#' Calculate Index of Glycemic Control
#'
#' @description
#' The function igc produces IGC values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' igc(data, lower = 70, upper = 140)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of IGC.
#'
#' @param lower Lower bound used for hypoglycemia cutoff. Default is 70
#' @param upper Upper bound used for hyperglycemia cutoff. Default is 140.
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' IGC is calculated by taking the sum of the Hyperglycemia
#' Index and the Hypoglycemia index. See hypo_index and hyper_index.
#'
#' Wrapping as.numeric() around the igc call on a dataset with
#' a single subject will return a numeric value corresponding to the
#' IGC value. This will not work for datasets with multiple subjects.
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
#' igc(example_data_1_subject)
#' igc(example_data_1_subject, lower = 80, upper = 160)
#'
#' data(example_data_5_subject)
#' igc(example_data_5_subject)
#' igc(example_data_5_subject, lower = 75, upper = 150)
#'

igc <- function(data, lower = 70, upper = 140){
  hyper = hypo = gl = id = NULL
  rm(list = c("gl", "id", "hyper", "hypo"))
  data = check_data_columns(data)
  is_vector = attr(data, "is_vector")

  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      hyper = sum((gl[gl > upper] - upper) ^ 1.1, na.rm = TRUE)/(length(!is.na(gl)) * 30),
      hypo = sum((lower - gl[gl < lower]) ^ 2, na.rm = TRUE)/(length(!is.na(gl)) * 30)
      ) %>%
    dplyr::mutate(igc = hyper + hypo) %>%
    dplyr::select(-hyper, -hypo)
  if (is_vector) {
    out$id = NULL
  }
  return(out)
}

