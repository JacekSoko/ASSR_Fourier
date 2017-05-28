#' @title Running Shiny appllication
#'
#' @description Function \code{FourierReadCSV} reads data from CSV file with format... see....
#'
#'
#' @return Run a Shiny app.
#'
#' @importFrom shiny runApp
#'
#' @export
ShinyRun <- function(){
  appDir <- system.file("shiny", package = "ASSRFourier")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing package.", call. = FALSE)
  }

  runApp(appDir, display.mode = "normal")
}
