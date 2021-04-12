#' Data Pre-Processor
#'
#' @description A helper function to assist in pre-processing the user-supplied
#' input data for use with other functions.
#' Typically, this function will process the data and return another dataframe.
#' This function ensures that the supplied data will be compitable with every
#' function within the \code{iglu} package. See Vignette for further details.
#'
#' @usage process_data(data, id, timestamp, glu, time_parser = as.POSIXct)
#'
#' @param data User-supplied dataset containing continuous glucose monitor data. Must
#' contain data for time and glucose readings at a minimum. Accepted formats are dataframe and tibble.
#'
#' @param id Optional column name (character string) corresponding to subject id column.
#' If no value is passed, an id of 1 will be assigned to the data.
#'
#' @param timestamp Required column name (character string) corresponding to time values in data. The dates can be
#' in any format parsable by as.POSIXct, or any format accepted by the parser passed to time_parser. See time_parser param for an explanation
#' on how to handle arbitrary formats.
#'
#' @param glu Required column name (character string) corresponding to blood glucose values, mg/dl
#'
#' @param time_parser Optional function used to convert datetime strings to time objects. Defaults to as.POSIXct.
#' If your times are in a format not parsable by as.POSIXct, you can parse a custom format by passing
#' function(time_string) \{strptime(time_string, format = <format string>)\} as the time_parser parameter.
#'
#' @details A dataframe with the columns "id", "time", and "gl" will be returned.
#' If there is a mention of "mmol/l" in the glucose column name, the glucose values will be multipled by 18 to convert to mg/dl
#' Based on John Schwenck's data_process for his bp package
#' https://github.com/johnschwenck/bp
#'
#' @return A processed dataframe object that cooperates with every other
#' function within the iglu package - all column names and formats comply.
#' @export
#'
#' @examples
#' data("example_data_1_subject")
#'
#' # Process example data
#' processed <- process_data(example_data_1_subject, id = "id", timestamp = "time", glu = "gl", time_parser = as.POSIXct)
#'
#' processed
#'
#' data("example_data_5_subject")
#'
#' # Process example data
#' processed_multisubject <- process_data(example_data_5_subject, id = "id", timestamp = "time", glu = "gl", time_parser = as.POSIXct)
#'
#' processed_multisubject
#'

process_data = function(data,
                        id = NULL,
                        timestamp = NULL,
                        glu = NULL,
                        time_parser = as.POSIXct) {
  if (is.data.frame(data) == FALSE) {
    if (tibble::is_tibble(data) == FALSE) {
      if (is.vector(data) == FALSE) {
        stop("Invalid data type, please use dataframe, tibble, or if passing just glucose readings, vector.")
      }
    }
  }
  data = na.omit(data)
  if (is.data.frame(data) || tibble::is_tibble(data)) {
    colnames(data) = tolower(colnames(data))
    data = na.omit(data)
    if (is.null(id)) {
      message("No 'id' parameter passed, defaulting id to 1")
      id = 1
    }
    if (is.character(id)) {
      if (tolower(id) %in% colnames(data) == FALSE) {

        warning("Could not find user-defined id argument name in dataset.\nFor example, if the user defines id=\"Subjects\" but no column is named \"Subjects\"\nThen there will be no matches for \"Subject\"\nCheck spelling of id argument.\nIf the dataset is only for one subject and has no id column, set single_subject = TRUE.")

        if(length(grep(paste("\\bid\\b", sep = ""), names(data))) == 1) {

          stop('Fix user-defined argument name for id. \nNote: A column in the dataset DOES match the name "id": \nIf this is the correct column, indicate as such in function argument. \ni.e. id = "id"\nIf the dataset is only for one subject and has no id column, set single_subject = TRUE.')

        }
      } else {

        col_idx <- grep(paste("\\b",tolower(id),"\\b", sep = ""), names(data))
        data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

        if(colnames(data)[1] != "id"){

          colnames(data)[1] <- "id"
          data$id <- as.character(data$id)
        }
      }
    } else {
      stop('User-defined id name must be character.\n')
    }

    if (is.character(timestamp)) {
      if (tolower(timestamp) %in% colnames(data) == FALSE) {

        warning("Could not find user-defined timestamp argument name in dataset.\nFor example, if the user defines timestamp=\"time\" but no column is named \"time\"\nThen there will be no matches for \"time\"\nCheck spelling of timestamp argument.")

        if(length(grep(paste("\\btime\\b", sep = ""), names(data))) == 1) {

          stop('Fix user-defined argument name for timestamp. \nNote: A column in the dataset DOES match the name "time": \nIf this is the correct column, indicate as such in function argument. \ni.e. timestamp = "time"')

        }
      } else {

        col_idx <- grep(paste("\\b",tolower(timestamp),"\\b", sep = ""), names(data))
        data <- data[, c(1, col_idx, (2:ncol(data))[-col_idx+1])]

        if(colnames(data)[2] != "time"){

          colnames(data)[2] <- "time"
        }

          tryCatch({
            data$time <- time_parser(data$time)
            },error=function(cond) {
              message("Failed to parse times, ensure times are in convertable format and possible.\nSee docs for explanation on how to handle arbitary formats.\nOriginal error message:")
              message(cond)
              stop("Error in time conversion.")
              return(NA)
            })

        }
    } else {
      stop('User-defined timestamp name must be character.\n')
    }

    if (is.character(glu)) {
      if (tolower(glu) %in% colnames(data) == FALSE) {

        warning("Could not find user-defined glucose argument name in dataset.\nFor example, if the user defines glu=\"glucose\" but no column is named \"glucose\"\nThen there will be no matches for \"glucose\"\nCheck spelling of glu argument.")

        if(length(grep(paste("\\bgl\\b", sep = ""), names(data))) == 1) {

          stop('Fix user-defined argument name for glucose. \nNote: A column in the dataset DOES match the name "gl": \nIf this is the correct column, indicate as such in function argument. \ni.e. glu = "glucose"')

        }
      } else {
        mmol = FALSE
        col_idx <- grep(paste("\\b",tolower(glu),"\\b", sep = ""), names(data))
        if (sum(grep("mmol/l", glu))) {
          mmol = TRUE
        }
        data <- data[, c(1:2, col_idx, (3:ncol(data))[-col_idx+2])]

        if(colnames(data)[3] != "gl"){
          colnames(data)[3] <- "gl"
        }
        data$gl <- as.numeric(data$gl)
        if (mmol) {
          data$gl = 18*data$gl
        }
        if (min(data$gl, na.rm = T) < 20) {
          warning("Minimum glucose reading below 20. Data may not be cleaned.")
        }
        if (max(data$gl, na.rm = T) > 500) {
          warning("Maximum glucose reading above 500. Data may not be cleaned.")
        }
      }
    } else {
      stop('User-defined glucose name must be character.\n')
    }

  }
  return(data)

}
