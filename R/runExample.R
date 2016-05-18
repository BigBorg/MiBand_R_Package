#' @title  MiBand Example
#' @description  an interative tutorial
#' @export
#' @author Borg
runExample <- function() {
  appDir <- system.file("shiny-examples", "MiBandApp", package = "MiBand")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `MiBand`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

