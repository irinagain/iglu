#' Functions to read raw data from a variety of common sensors. Assumes .csv format for all data. Due to limited accessiblity of data,
#' only the import for libre sensors has been tested on actual data as of 4/7/2021.
#'
#' Heavily derived from the readers available in the cgmanalysis package's cleandata function.
#'
#' @description Helper function to assist in reading data directly from sensor outputs. Should return a dataframe in correct format
#' for use with the rest of the \code{iglu} package. Assumes all data will be readable with base R read.csv function.
#'
#' @usage read_raw_data("cgmdata.csv", sensor = "libre", id = "read")
#'
#' @param filename String matching the name of the data to be read. Assumed to be .csv
#'
#' @param sensor String naming the type of sensor the data was exported from.
#' Currently supports "dexcom", "libre", "librepro", "asc", and "ipro" as options.
#'
#' @param id String indicating subject id. Defaults to "filename".
#' A value of "read" will cause the program to attempt to read the subject id from the file. A value of "filename" will cause the
#' program to use the basename of the filename (i.e. filename without any directory information) with .csv removed as subject id.
#' The asc reader currently does not support id="read"
#'
#' @return A dataframe containing the data read from the named file.
#'
#' @export
#'
#' @details A dataframe object with the columns "id", "time" and "gl" and one row per reading will be returned.
#'
#' @references
#' Vigers et al. (2019) cgmanalysis: An R package for descriptive analysis of continuous glucose monitor data
#' \emph{PLoS ONE} \strong{14(10)}: e0216851,
#' \doi{10.1371/journal.pone.0216851}
#'
#' @examples
#' read_raw_data("johndoe.csv", sensor = "dexcom", id = "filename")
#'
#' read_raw_data("yourdata.csv", sensor = "libre", id = "read")
#'
#' read_raw_data("data.csv", sensor = "librepro", id = "read")
#'
#' read_raw_data("home/janedoe.csv", sensor = "asc", id = "filename")
#'
#' read_raw_data("cgmsequences.csv", sensor = "ipro", id = "read")
#'

read_raw_data = function(filename, sensor = NULL, id = "filename") {
  if (is.null(sensor)) {
    stop("You must enter the sensor type to be read from. Current supported sensors are 'dexcom', 'libre', 'librepro', 'asc', 'ipro'")
  }
  sensor = tolower(sensor)

  importdexcom = function(filename, id= "read") {
    data = read.csv(filename, stringsAsFactors = FALSE)
    if (tolower(id) == "read") {
      id <- data[3,grep("patient",tolower(colnames(data)))]
    } else if (tolower(id) == "filename") {
     id = sub("\\..*","",basename(filename))
    }
    out = data[,c(grep("timestamp",tolower(colnames(data))),grep("glucose",tolower(colnames(data)))[1])]
    colnames(out) = c("time", "gl")
    out$id = id

    out$time <- strptime(out$time, format='%Y-%m-%dT%H:%M:%S', tz = "")
    out = out[,c(3,1,2)]

    return(out)
  }

  importlibre = function(filename, id="read", mmol = F,colnamerow = 2) {
    data = read.csv(filename, stringsAsFactors = FALSE)
    if (tolower(id) == "read") {
      id <- data[1,1]
    } else if (tolower(id) == "filename") {
      id = sub("\\..*","",basename(filename))
    }
    colnames(data) = data[colnamerow,]
    data = data[(colnamerow+1):length(data[,1]),]
    data <- data[,c(grep("timestamp",tolower(colnames(data))),
                      grep("historic glucose",tolower(colnames(data))))]
    colnames(data) <- c('time','gl')
    data$id = id
    if (mmol) {
      data$gl = 18*data$gl
    }
    return(data)
  }

  importlibrepro = function(filename, id="read") {
    data = read.csv(filename, stringsAsFactors = FALSE)
    if (tolower(id) == "read") {
      id <- data[1,1]
    } else if (tolower(id) == "filename") {
      id = sub("\\..*","",basename(filename))
    }
    colnames(data) <- data[2,]
    data <- data[-c(1:2),]
    data <- data[,c("Time","Historic Glucose (mg/dL)")]
    colnames(data) <- c('time','gl')
    data$id = id
    return(data)
  }

  importasc = function(filename, id="filename") {
    data = read.csv(filename, stringsAsFactors = FALSE)
    if (tolower(id) == "filename") {
      id = sub("\\..*","",basename(filename))
    }
    if (tolower(id) == "read") {
      stop("ASC sensor reader does not support reading id from file. Call again with id='filename' or id=<subject name>")
    }
    data$timestamp = paste(data$Date,data$Time)
    data$sensorglucose = data$Value
    data = data[,c('time','gl')]
    data$id = id
    return(data)
  }


  importipro = function(filename, id="read") {
    data = read.csv(filename, stringsAsFactors = FALSE)
    base::colnames(data) <- data[11,]
    if (tolower(id) == "read") {
      id <- data[2,2]
    } else if (tolower(id) == "filename") {
      id <- sub("\\..*","",basename(filename))
    }
    data <- data[-c(1:11),]
    if (grepl("- | /",data$Timestamp[1]) == F) {
      data$Timestamp <- base::as.POSIXct(as.numeric(data$Timestamp)* (60*60*24),
                                          origin = "1899-12-30",
                                          tz = "UTC")
    }
    data$Timestamp <- base::sub("[.]00","",data$Timestamp)
    data <- data[,c("time","gl")]
    base::colnames(data) <- c('time','gl')
    data$id = id
    return(data)
  }


  out = switch(sensor,
         "dexcom" = importdexcom(filename, id = id),
         "libre" = importlibre(filename, id = id),
         "librepro" = importlibrepro(filename, id = id),
         "asc" = importasc(filename, id = id),
         "ipro" = importipro(filename, id = id))

  return(out)
}

