#' @title The main menu for the DSAIDE package
#'
#' @description This function opens a Shiny App menu that will allow the user to run the different simulation apps
#'
#' @details Run this function with no arguments to start the main menu (a shiny App) for DSAIDE 
#' @examples
#' \dontrun{dsaidemenu()}
#' @author Andreas Handel, Sina Solaimanpour
#' @import shiny
#' @importFrom knitr knit
#' @export

dsaidemenu <- function() {

  cond <- 1

    while (cond == 1){
    
    appname <- NULL
    appDir <- system.file("shinyapps", "MainMenu", package = "DSAIDE")
    op = shiny::runApp(appDir = appDir)
    
    if (op == "X") {cond = 0} #leave while loop/menu
    if (op == "A") {appname = "IDDynamicsIntro"}
    if (op == "B") {appname = "CharacteristicsofID"}
    if (op == "C") {appname = "IDPatterns"}
    if (op == "D") {appname = "ReproductiveNumber"}
    if (op == "E") {appname = "DirectTransmission"}
    if (op == "F") {appname = "EnvironmentalTransmission"}
    if (op == "G") {appname = "VectorTransmission"}
    if (op == "H") {appname = "IDControl"}
    if (op == "I") {appname = "HostHeterogeneity"}
    if (op == "J") {appname = "StochasticDynamics"}
    if (op == "K") {appname = "EvolutionaryDynamics"}
    
    if (!is.null(appname))     #run the shiny app chosen
    {
        appDir <- system.file("shinyapps", appname, package = "DSAIDE")
        shiny::runApp(appDir = appDir)
    }

  }
  print('*************************************************')
  print('Exiting the DSAIDE main menu.')
  print('I hope you had a fun and educational experience!')
  print('*************************************************')
  
}

.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to the DSAIDE package. Type dsaidemenu() to get started.")
}

