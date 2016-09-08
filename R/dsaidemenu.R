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
    
    cat("Please choose an option (upper or lower case works):", "\n\n")

    cat("\tA - ID Dynamics Introduction App", "\n")
    cat("\tB - Characterizing ID States App", "\n")
    cat("\tC - ID Patterns App", "\n")
    cat("\tD - Reproductive Number App", "\n")
    cat("\tE - Modes of Direct Transmission App", "\n")
    cat("\tF - Environmental Transmission App", "\n")
    cat("\tG - Vector-borne Transmission App", "\n")
    cat("\tH - ID Control App", "\n")
    cat("\tI - Host Heterogeneity App", "\n")
    cat("\tJ - Stochastic Dynamics App", "\n")
    cat("\tK - Evolutionary Dynamics App", "\n")
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

