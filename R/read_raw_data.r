#' Read raw data from a variety of common sensors.
#'
#' @description Helper function to assist in reading data directly from sensor outputs. Should return a dataframe in correct format
#' for use with the rest of the \code{iglu} package. Assumes all data will be readable with base R read.csv function.
#'
#' @param filename String matching the name of the data to be read. Assumed to be .csv
#'
#' @param sensor String naming the type of sensor the data was exported from.
#' Must be one of "dexcom", "libre", "librepro", "asc", or "ipro".
#'
#' @param id String indicating subject id. Defaults to "filename".
#' A value of "read" will cause the program to attempt to read the subject id from the file. A value of "filename" will cause the
#' program to use the basename of the filename (i.e. filename without any directory information) with .csv removed, as subject id.
#' A value of "default" will cause the program to use whichever of "read" or "filename" that is default for that specific sensor.
#' Any other string will be treated as the unique id for the entire file.
#'
#' Note the asc reader currently does not support id="read"
#' @inheritParams CGMS2DayByDay
#'
#' @return A dataframe containing the data read from the named file.
#'
#' @export
#'
#' @author David Buchanan
#'
#' @details A dataframe object with the columns "id", "time" and "gl" and one row per reading will be returned. For the libre reader,
#' if the phrase "mmol/l" is found in the column names, the glucose values will be multiplied by 18.
#' Assumes .csv format for all data.
#' Sensor formats change with ongoing development, so these functions may become depreciated.
#' If any issues are encountered, contact the package maintainer. This is currently Irina Gaynanova,
#' who can be reached at \email{irinag@@stat.tamu.edu}
#' Heavily derived from the readers avaiable in the cgmanalysis package's cleandata function.
#'
#' @references
#' Vigers et al. (2019) cgmanalysis: An R package for descriptive analysis of continuous glucose monitor data
#' \emph{PLoS ONE} \strong{14(10)}: e0216851,
#' \doi{10.1371/journal.pone.0216851}
#'
#'

read_raw_data = function(filename, sensor = c("dexcom", "libre", "librepro", "asc", "ipro"),
                         id = "filename", tz = "") {

  if (length(sensor) > 1) {
    warning("Sensor type not specified, using dexcom as default")
  }
  sensor = match.arg(sensor)

  if (is.null(sensor)) {
    stop("You must enter the sensor type to be read from. Current supported sensors are 'dexcom', 'libre', 'librepro', 'asc', 'ipro'")
  }

  importdexcom = function(filename, id= "read", tz) {
    data = utils::read.csv(filename, stringsAsFactors = FALSE)
    if (tolower(id) == "read") {
      id <- data[2,grep("subtype",tolower(colnames(data)))]
    } else if (tolower(id) == "filename") {
     id = sub("\\.csv*","",basename(filename))
    }
    # remove rows that are not EGV (electronic glucose value?)
    header = data[, grep("event.type", tolower(colnames(data)))] != "EGV"
    out = data[!header,
               c(grep("timestamp",tolower(colnames(data))),
                 grep("glucose",tolower(colnames(data)))[1])]
    colnames(out) = c("time", "gl")
    out$id = id
    out$gl = as.numeric(out$gl)
    out$time <- as.POSIXct(out$time, format='%Y-%m-%dT%H:%M:%S', tz = tz)
    out = out[,c(3,1,2)]

    return(out)
  }

  importlibre = function(filename, id="read",colnamerow = 2, tz) {
    data = utils::read.csv(filename, stringsAsFactors = FALSE)
    mmol = FALSE
    if (tolower(id) == "read") {
      id <- data[1,1]
    } else if (tolower(id) == "filename") {
      id = sub("\\.csv*","",basename(filename))
    }
    colnames(data) = data[colnamerow,]
    data = data[(colnamerow+1):length(data[,1]),]

    if (sum(grep("mmol/l", tolower(colnames(data))))) {
      mmol = TRUE
    }

    data <- data[,c(grep("timestamp",tolower(colnames(data))),
                      grep("historic glucose",tolower(colnames(data))))]
    colnames(data) <- c('time','gl')
    data$gl = as.numeric(data$gl)
    data$id = id
    data$time <- as.POSIXct(data$time, format='%Y-%m-%dT%H:%M:%S', tz = tz)
    data = data[,c(3,1,2)]
    if (mmol) {
      data$gl = 18*data$gl
    }
    return(data)
  }

  importlibrepro = function(filename, id="read", tz) {
    data = utils::read.csv(filename, stringsAsFactors = FALSE)
    if (tolower(id) == "read") {
      id <- data[1,1]
    } else if (tolower(id) == "filename") {
      id = sub("\\.csv*","",basename(filename))
    }
    colnames(data) <- data[2,]
    data <- data[-c(1:2),]
    data <- data[,c("Time","Historic Glucose (mg/dL)")]
    colnames(data) <- c('time','gl')
    data$id = id

    # reformat data types
    data$gl = as.numeric(data$gl)
    data$time = as.POSIXct(data$time, format='%m/%d/%Y %H:%M', tz = tz)

    data = data[,c(3,1,2)]
    return(data)
  }

  importasc = function(filename, id="filename", tz) {
    data = utils::read.csv(filename, stringsAsFactors = FALSE)
    if (tolower(id) == "filename") {
      id = sub("\\.csv*","",basename(filename))
    }
    if (tolower(id) == "read") {
      stop("ASC sensor reader does not support reading id from file. Call again with id='filename' or id=<subject name>")
    }
    data$timestamp = paste(data$Date,data$Time)
    data$sensorglucose = data$Value
    data = data[,c('time','gl')]
    data$id = id
    data$gl = as.numeric(data$gl)
    data$time <- as.POSIXct(data$time, format='%Y-%m-%dT%H:%M:%S', tz = tz)
    data = data[,c(3,1,2)]
    return(data)
  }


  importipro = function(filename, id="read", tz) {
    data = utils::read.csv(filename, stringsAsFactors = FALSE)
    base::colnames(data) <- data[11,]
    if (tolower(id) == "read") {
      id <- data[2,2]
    } else if (tolower(id) == "filename") {
      id <- sub("\\.csv*","",basename(filename))
    }
    data <- data[-c(1:11),]
    if (grepl("- | /",data$Timestamp[1]) == F) {
      data$Timestamp <- as.POSIXct(data$Timestamp, format = "%m/%d/%y %H:%M", tz = tz)
    }
    data <- data[,c("Timestamp","Sensor Glucose (mg/dL)")]
    base::colnames(data) <- c('time','gl')
    data$id = id
    data$gl = as.numeric(data$gl)
    data = data[,c(3,1,2)]
    return(data)
  }

  if (id == "default") {
    out = switch(sensor,
                 "dexcom" = importdexcom(filename, tz = tz),
                 "libre" = importlibre(filename, tz = tz),
                 "librepro" = importlibrepro(filename, tz = tz),
                 "asc" = importasc(filename, tz = tz),
                 "ipro" = importipro(filename, tz = tz))

  } else {
    out = switch(sensor,
                 "dexcom" = importdexcom(filename, id = id, tz = tz),
                 "libre" = importlibre(filename, id = id, tz = tz),
                 "librepro" = importlibrepro(filename, id = id, tz = tz),
                 "asc" = importasc(filename, id = id, tz = tz),
                 "ipro" = importipro(filename, id = id, tz = tz))

  }

  return(out)
}

