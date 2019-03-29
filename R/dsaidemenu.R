#' @title The main menu for the DSAIDE package
#'
#' @description This function opens a Shiny app with a menu that will allow the user to run the different simulations.
#'
#' @details Run this function with no arguments to start the main menu (a Shiny app) for DSAIDE
#' @examples
#' \dontrun{dsaidemenu()}
#' @author Andreas Handel
#' @import shiny
#' @export

dsaidemenu <- function() {


    appDir <- system.file( "DSAIDE", package = "DSAIDE") #get directory for main menu app
    shiny::runApp(appDir = appDir, launch.browser = TRUE) #run main menu app

    print('*************************************************')
    print('Exiting the DSAIDE main menu.')
    print('I hope you had a fun and educational experience!')
    print('*************************************************')
}

#needed to prevent NOTE messages on CRAN checks
#most of those are from the ggplot commands in the generate_ functions,
#the last one is from the subset function in the fit functions
utils::globalVariables(c("xvals", "yvals", "varnames","IDvar","style","Condition", "simfunction"))


.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to the DSAIDE package. Type dsaidemenu() to get started.")
}
