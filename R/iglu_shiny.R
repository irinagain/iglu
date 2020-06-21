#' Run IGLU Shiny App
#'
#' @export
#'
# https://deanattali.com/2015/04/21/r-package-shiny-app/
iglu_shiny <- function() {
  appDir <- system.file('shiny_iglu', package = 'iglu')

  shiny::runApp(appDir, display.mode = 'normal')
}
