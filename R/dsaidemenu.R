#' @title The main menu for the DSAIDE package
#'
#' @description This function opens a Shiny App menu that will allow the user to run the different simulation apps
#'
#' @details Run this function with no arguments to start the main menu (a shiny App) for DSAIDE 
#' @examples
#' \dontrun{dsaidemenu()}
#' @author Andreas Handel
#' @export

dsaidemenu <- function() {
  cond <- 1
  while (cond == 1)
  {
    appname <- NULL
    appDir <- system.file("shinyapps", "MainMenu", package = "DSAIDE")
    appname = shiny::runApp(appDir = appDir)
    if (!is.null(appname) & appname != "Exit")     #run the shiny app chosen
    {
      appDirname <- system.file("shinyapps", appname, package = "DSAIDE")
      shiny::runApp(appDir = appDirname)
    }
    if (appname == "Exit") {cond = 0} #leave while loop/menu
  }
  print('*************************************************')
  print('Exiting the DSAIDE main menu.')
  print('I hope you had a fun and educational experience!')
  print('*************************************************')
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to the DSAIDE package. Type dsaidemenu() to get started.")
}