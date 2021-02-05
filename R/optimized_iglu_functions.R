#' Optimized Calculations of Time Dependent iglu Metrics
#'
#' @description
#' The function optimized_iglu_functions optimizes the calculation of all time dependent iglu
#' metrics by extracting the CGMS2DayByDay calculation and passing the result into each function.
#'
#' @usage
#' optimized_iglu_functions(data)
#'
#' @param data DataFrame object with column names "id", "time", and "gl".
#'
#' @return
#' If a data.frame object is passed, then a tibble object with 1 row for each subject
#' and one column for each metric is returned.
#'
#' @export
#'
#' @details
#' Returns a tibble object with 1 row for each subject and a column for each metric. This
#' function includes time dependent iglu metrics only. For metric specific information,
#' please see the corresponding function documentation.
#'
#' @examples
#' data(example_data_1_subject)
#' optimized_iglu_functions(example_data_1_subject)
#'
#' data(example_data_5_subject)
#' optimized_iglu_functions(example_data_5_subject)
#'


optimized_iglu_functions <- function(data) {

  gl = id = NULL
  rm(list = c("gl", "id"))
  data = check_data_columns(data)

  ## Passes CGMS2DayByDay data to individual functions
  function_call <- function(data, hours) {
    conga_single_O <- function(.data_ip, hours = 24, tz = "") {
      gl_by_id_ip = .data_ip[[1]]
      dt0 = .data_ip[[3]]
      hourly_readings = round(60 / dt0, digits = 0)

      return(sd(diff(as.vector(t(gl_by_id_ip)), lag = hourly_readings * hours), na.rm = TRUE))
    }

    gvp_single_O <- function(.data_ip) {
      daybyday = as.vector(t(.data_ip[[1]]))
      reading_gap = .data_ip[[3]]
      diffvec = diff(daybyday, na.rm = T)
      added_length = sqrt(reading_gap^2+diffvec^2)
      base_length = length(na.omit(diffvec))*reading_gap

      return(sum(added_length, na.rm = T)/sum(base_length, na.rm = T))
    }

    modd_single_O <- function(.data_ip, lag = 1) {
      gl_by_id_ip = .data_ip[[1]]

      return(mean(abs(diff(gl_by_id_ip, lag = lag)), na.rm = TRUE))
    }

    sd_roc_O <- function(.data_ip, timelag = 15, dt0 = NULL, inter_gap = 45, tz = "") {

      gl_ip_vec = as.vector(t(.data_ip[[1]]))
      dt0 = .data_ip[[3]]
      roc = c(rep(NA, timelag/dt0),
              diff(gl_ip_vec, lag = timelag/dt0)/timelag)

      return(sd(roc, na.rm = TRUE))
    }

    cv_measures_mean_O <- function(.data_ip, dt = NULL, inter_gap = 45, tz = "") {
      cv<- function(data, na.rm = FALSE ){
        return((sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE))*100)
      }

      .data_ip$gd2d %>%
        apply( 1, cv, na.rm = TRUE) %>%
        mean( na.rm = TRUE)
    }

    cv_measures_sd_O <- function(.data_ip, dt = NULL, inter_gap = 45, tz = "") {
      cv<- function(data, na.rm = FALSE ){
        return((sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE))*100)
      }

      .data_ip$gd2d %>%
        apply( 1, cv, na.rm = TRUE) %>%
        sd( na.rm = TRUE)
    }

    auc_single_O <- function(.data_ip, tz = "") {

      each_area = daily_area = NULL
      rm(list = c("each_area", "daily_area"))

      dt0 = .data_ip$dt0

      day <- rep(.data_ip$actual_dates, 1440/dt0)
      gl <- as.vector(t(.data_ip$gd2d))

      temp_df = cbind.data.frame(day, gl) %>%
        dplyr::group_by(day) %>%
        dplyr::summarise(each_area = (dt0/60) * ((gl[2:length(gl)] + gl[1:(length(gl)-1)])/2)) %>%
        dplyr::summarise(daily_area = sum(each_area, na.rm = TRUE),
                         hours = dt0/60 * length(na.omit(each_area)),
                         hourly_avg = daily_area/hours, .groups = 'drop')

      return(mean(temp_df$hourly_avg))
    }

    mag_single_O <- function(.data_ip) {
      n = 60

      data_ip = .data_ip
      idx = seq(1, ncol(data_ip[[1]]), by = round(n/data_ip[[3]]))
      idx_gl = as.vector(t(data_ip[[1]][, idx]))
      mag = sum(abs(diff(idx_gl)), na.rm = TRUE)/
        (length(na.omit(idx_gl))*n/60)

      return(mag)
    }

    SdW <- function(.data_ip) {
      out = mean(apply(.data_ip$gd2d, 1, sd, na.rm = TRUE), na.rm = TRUE)
      return(out)
    }

    SdHHMM <- function(.data_ip) {

      SdHHMM = sd(apply(.data_ip$gd2d, 2, mean, na.rm = TRUE), na.rm = TRUE)

      return(SdHHMM)
    }

    SdWSH <- function(.data_ip) {
      dt0 = .data_ip$dt0
      win = round(60/dt0) # how many measurements are within 1 hour
      gs = as.vector(t(.data_ip$gd2d))
      SdWSH = mean(caTools::runsd(gs, k = win, endrule = "trim"), na.rm = TRUE)

      return(SdWSH)
    }

    SdDM <- function(.data_ip) {
      meanR = apply(.data_ip$gd2d, 1, mean, na.rm = TRUE)
      SdDM = sd(meanR, na.rm = TRUE)

      return(SdDM)
    }

    SdB <- function(.data_ip) {
      SdB = mean(apply(.data_ip$gd2d, 2, sd, na.rm = TRUE), na.rm = TRUE)
    }

    SdBDM <- function(.data_ip) {
      meanR = apply(.data_ip$gd2d, 1, mean, na.rm = TRUE)
      # SdBDM - between days, within timepoints, corrected for changes in daily means
      med = matrix(rep(meanR, each = ncol(.data_ip$gd2d)), ncol = ncol(.data_ip$gd2d), byrow = TRUE)
      SdBDM = mean(apply(.data_ip$gd2d - med, 2, sd, na.rm = TRUE), na.rm = TRUE)
    }

    .data_ip = CGMS2DayByDay(data, tz = "")
    out <- data.frame("Conga" = numeric(), "GVP" = numeric(), "MODD" = numeric(),
                      "SD Roc" = numeric(), "CV_Measures_Mean" = numeric(),
                      "CV_Measures_SD" = numeric(), "AUC" = numeric(), "MAG" = numeric(),
                      "SDw" = numeric(),  "SDhhmm" = numeric(), "SDwsh" = numeric(), "SDdm" = numeric(),
                      "SDb" = numeric(), "SDbdm" = numeric())

    outrow <- c(1:14)

    ## conga
    outrow[1] = conga_single_O(.data_ip)

    ## GVP
    outrow[2] = gvp_single_O(.data_ip)

    ## MODD
    outrow[3] = modd_single_O(.data_ip)

    ## SD Roc
    outrow[4] = sd_roc_O(.data_ip)

    ## CV Measures
    outrow[5] = cv_measures_mean_O(.data_ip)
    outrow[6] = cv_measures_sd_O(.data_ip)

    ## AUC
    outrow[7] = auc_single_O(.data_ip)

    ## MAG
    outrow[8] = mag_single_O(.data_ip)

    ## SD Measures
    outrow[9] = SdW(.data_ip)
    outrow[10] = SdHHMM(.data_ip)
    outrow[11] = SdWSH(.data_ip)
    outrow[12] = SdDM(.data_ip)
    outrow[13] = SdB(.data_ip)
    outrow[14] = SdBDM(.data_ip)

    out[1, ] <- outrow

    return(out)
  }

  id = NULL
  rm(id)

  ## Creates the tibble and calls the function above
  out = data %>%
    dplyr::filter(!is.na(gl)) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      function_call(data.frame(id,time,gl), hours = 24), .groups = "drop"
    )

  return(out)
}
