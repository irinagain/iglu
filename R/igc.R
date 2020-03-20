#' Calculate Index of Glycemic Control
#'
#' @description
#' The function igc produces IGC values in data.frame
#' form with one column and one row per subject.
#'
#' @usage
#' igc(data, LLTR = 80, ULTR = 140, a = 1.1, b = 2, c = 30, d = 30)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values. NA's will be omitted from the glucose
#' values in calculation of IGC.
#'
#' @inheritParams hyper_index
#' @inheritParams hypo_index
#'
#' @details
#' A dataframe structure with one column and a row for each subject.
#'
#' IGC is calculated by taking the sum of the Hyperglycemia
#' Index and the Hypoglycemia index. See hypo_index and hyper_index.
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
#' igc(example_data_1_subject, ULTR = 160)
#'
#' data(example_data_5_subject)
#' igc(example_data_5_subject)
#' igc(example_data_5_subject, LLTR = 75, ULTR = 150)
#'

igc <- function(data, LLTR = 80, ULTR = 140, a = 1.1, b = 2, c = 30, d = 30){

  out_hyper <- hyper_index(data, ULTR = ULTR, a = a, c = c)
  out_hypo <- hypo_index(data, LLTR = LLTR, b = b, d = d)

  out <- dplyr::inner_join(out_hyper, out_hypo)%>%
    dplyr::group_by(id)%>%
    dplyr::summarize(igc = hyper_index + hypo_index)

  return(out)
}

