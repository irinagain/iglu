#' Calculate Index of Glycemic Control
#'
#' @description
#' The function igc produces IGC values in a tibble object.
#'
#' @usage
#' igc(data, LLTR = 80, ULTR = 140, a = 1.1, b = 2, c = 30, d = 30)
#'
#' @param data DataFrame object with column names "id", "time", and "gl",
#' or numeric vector of glucose values.
#'
#' @inheritParams hyper_index
#' @inheritParams hypo_index
#'
#' @details
#' A tibble object with 1 row for each subject, a column for subject id and
#' a column for the IGC values is returned.
#'
#' IGC is calculated by taking the sum of the Hyperglycemia
#' Index and the Hypoglycemia index. See \code{\link{hypo_index}} and
#' \code{\link{hyper_index}}.
#'
#' @return A tibble object with two columns: subject id and corresponding IGC value.
#'
#' @export
#'
#' @references
#' Rodbard (2009) Interpretation of continuous glucose monitoring data:
#' glycemic variability and quality of glycemic control,
#' \emph{Diabetes Technology and Therapeutics} \strong{11} .55-67,
#' \doi{10.1089/dia.2008.0132}.
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
  id = igc = NULL
  rm(list = c("id", "igc"))

  data <- check_data_columns(data)
  is_vector <- attr(data, "is_vector")

  out_hyper <- hyper_index(data, ULTR = ULTR, a = a, c = c)
  out_hypo <- hypo_index(data, LLTR = LLTR, b = b, d = d)

  out <- dplyr::inner_join(out_hyper, out_hypo, by = "id")%>%
    dplyr::group_by(id)%>%
    dplyr::summarize(IGC = hyper_index + hypo_index)

  return(out)
}

