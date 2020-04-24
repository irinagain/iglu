#' Title
#'
#' @inheritParams conga
#'
#' @return
#' @export
#'
#' @examples
sd_measures <- function(data, dt0 = NULL, inter_gap = 45, tz = ""){

  subject = unique(data$id)
  ns = length(subject)

  # Calculate uniform grid for all subjects
  gdall = list()
  for (i in 1:ns){
    if (i != 1){
      dt0 = out$dt0
    }
    out = data %>%
      dplyr::filter(id == subject[i]) %>%
      CGMS2DayByDay(tz = tz, dt0 = dt0, inter_gap = inter_gap)
    gdall[[i]] <- out$gd2d
  }
  dt0 = out$dt0

  results = lapply(
    gdall,
    function(gd2d) {
      # SdW - vertical within days
      out = data.frame(id = NA, SdW = mean(apply(gd2d, 1, sd, na.rm = T), na.rm = T))
      # SdHHMM - between time points
      out$SdHHMM = sd(apply(gd2d, 2, mean, na.rm = T), na.rm = T)
      # SdWSH - Within series - for 1 hour window
      win = round(60/dt0)
      gs = as.vector(t(gd2d))
      N = length(gs)
      ind = rep(1:ceiling(N/win), each = win)[1:N]
      out$SdWSH = mean(tapply(gs, ind, sd, na.rm = T), na.rm = T)

      # SdDM - "Horizontal" sd
      meanR = apply(gd2d, 1, mean, na.rm = T)
      out$SdDM = sd(meanR, na.rm = T)

      # SdB - between days, within timepoints
      out$SdB = mean(apply(gd2d, 2, sd, na.rm = T), na.rm = T)

      # SdBDM - between days, within timepoints, corrected for changes in daily means
      med = matrix(rep(meanR, each = ncol(gd2d)), ncol = ncol(gd2d), byrow = T)
      out$SdBDM = mean(apply(sqrt((gd2d - med)^2), 1, mean, na.rm = T), na.rm = T)

      out
    })

  results = dplyr::bind_rows(results) %>%
    dplyr::mutate(id = unique(data$id))

  return(results)
}
