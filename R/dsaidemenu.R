#' @title The main menu for the DSAIDE package
#'
#' @description This function opens a menu that will allow the user to run the different simulation apps
#'
#' @details Run this function with no arguments to start the main menu for the DSAIDE shiny apps
#' @examples
#' \dontrun{dsaidemenu()}
#' @author Andreas Handel, Sina Solaimanpour
#' @import shiny
#' @importFrom knitr knit
#' @export


dsaidemenu <- function() {

  cond <- 1
  
  print('*************************************************')
  print('Welcome to  the DSAIDE main menu!')
  print(paste('This is DSAIDE version ', utils::packageVersion("DSAIDE"),", last updated ", utils::packageDescription('DSAIDE')$Date,sep=''))
  print('Have fun exploring the infectious disease models!')
  print('*************************************************')

  while (cond == 1){
    
    appname <- NULL
    
    cat("Please choose an option:", "\n\n")

    cat("\tA - ID Dynamics Introduction App", "\n")
    cat("\tB - Characterizing ID States App", "\n")
    cat("\tC - ID Patterns App", "\n")
    cat("\tD - Reproductive Number App", "\n")
    #cat("\t5 - Modes of Transmission App", "\n")
    #cat("\t6 - ID Control App", "\n")
    #cat("\t7 - Host Heterogeneity App", "\n")
    #cat("\t8 - Stochastic Dynamics App", "\n")
    #cat("\t9 - Evolutionary Dynamics App", "\n")
    cat("\tX - Exit", "\n")

    op = scan(what="character", n = 1)
    op = toupper(op)
    
    if (op == "X") {cond = 0} #leave while loop/menu
    if (op == "A") {appname = "IDDynamicsIntro"}
    if (op == "B") {appname = "CharacteristicsofID"}
    if (op == "C") {appname = "IDPatterns"}
    if (op == "D") {appname = "ReproductiveNumber"}
    if (op == "E") {appname = "DirectTransmission"}
    if (op == "F") {appname = "EnvironmentalTransmission"}
    if (op == "G") {appname = "VectorTransmission"}
    #if (op == 5) {appname = "TransmissionModes"}
    #if (op == 6) {appname = "IDControl"}
    #if (op == 7) {appname = "HostHeterogeneity"}
    #if (op == 8) {appname = "StochasticDynamics"}
    #if (op == 9) {appname = "EvolutionaryDynamics"}

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

