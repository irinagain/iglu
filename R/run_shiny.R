#' @export
#'

# https://deanattali.com/2015/04/21/r-package-shiny-app/
run_shiny <- function() {
  appDir <- system.file('inst', 'shiny_iglu', package = 'iglu')

  shiny::runApp(appDir, display.mode = 'external')
}
