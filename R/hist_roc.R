#' Plot histogram of Rate of Change values (ROC)
#'
#' @description
#' The function hist_roc produces a histogram plot of ROC values
#'
#' @usage
#' hist_roc(data, subjects = NULL, timelag = 15, tz = "")
#'
#' @inheritParams conga
#'
#' @param subjects String or list of strings corresponding to subject names
#' in 'id' column of data. Default is all subjects.
#'
#' @param timelag Integer indicating the time period (# minutes) over which rate
#' of change is calculated. Default is 15, e.g. rate of change is the change in
#' glucose over the past 15 minutes divided by 15.
#'
#' @return A histogram of ROC values per subject
#'
#' @export
#'
#' @details
#' For the default, a histogram is produced for each subject displaying the ROC values
#' colored by ROC categories defined as follows. The breaks for the categories are:
#' c(-Inf, -3, -2, -1, 1, 2, 3, Inf) where the glucose is in mg/dl and the ROC values are in mg/dl/min.
#' A ROC of -5 mg/dl/min will thus be placed in the first category and colored accordingly.
#'
#' @seealso \code{\link{plot_roc}} for reference paper on ROC categories.
#'
#' @author Elizabeth Chun, David Buchanan
#'
#' @references
#' Clarke et al. (2009) Statistical Tools to Analyze Continuous Glucose Monitor Data,
#' Diabetes
#' \emph{Diabetes Technology and Therapeutics} \strong{11} S45-S54,
#' \doi{10.1089/dia.2008.0138}.
#'
#' @examples
#'
#' data(example_data_1_subject)
#' hist_roc(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' hist_roc(example_data_5_subject)
#' hist_roc(example_data_5_subject, subjects = 'Subject 3')
#'

hist_roc <- function(data, subjects = NULL, timelag = 15, tz = "") {

  gl = id = roc = category = NULL
  rm(list = c("gl", "id", "roc", "category"))
  data = check_data_columns(data)

  if (!is.null(subjects)) {
    data <- data[data$id %in% subjects, ]
  }

  data = data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      roc = roc(data.frame(id, time, gl), timelag, tz)$roc,
      category = cut(roc, breaks = c(-Inf, -3, -2, -1, 1, 2, 3, Inf),
                     labels = c("-Inf to -3", "-3 to -2", "-2 to -1",
                                "-1 to 1", "1 to 2", "2 to 3", "3 to Inf"))
    )

  colours = c("-Inf to -3" = "#0025FA", "-3 to -2" = "#197DE3",
              "-2 to -1" = "#B3FFF8", "-1 to 1" = "white",
              "1 to 2" = "#FEC7B6", "2 to 3" = "#FB5454",
              "3 to Inf" = "#9F0909")
  ggplot2::ggplot(data, ggplot2::aes(roc, fill = category)) +
    ggplot2::geom_histogram(binwidth = 0.1, alpha = 0.72, na.rm = TRUE) +
    ggplot2::facet_wrap(~id, scales = "free_x") +
    ggplot2::scale_fill_manual(values = colours, name = "Category (mg/dl/min)")
}
