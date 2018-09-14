#' @title A helper function that takes a model and generates the shiny UI elements for it output
#'
#' @description This function generates input buttons and sliders for a supplied model.
#' This is a helper function called by the shiny app.
#' @param model a modelbuilder model structure
#' @param output shiny output structure
#' @return HTML formatted text for display in a Shiny UI
#' @details This function is called by the Shiny server to produce the Shiny input UI elements.
#' @author Andreas Handel
#' @export

generate_shinyinput <- function(model, output)
{
    ###########################################
    #server part that dynamically creates the UI
    output$vars <- renderUI({
        nvars = length(model$var)  #number of variables/compartments in model
        allv = lapply(1:nvars, function(n) {
            numericInput(model$var[[n]]$varname,
                         model$var[[n]]$vartext,
                         value = model$var[[n]]$varval)
        })
        do.call(mainPanel, allv)
    })

    output$pars <- renderUI({
        npars = length(model$par)  #number of parameters in model
        allp = lapply(1:npars, function(n) {
            numericInput(
                model$par[[n]]$parname,
                model$par[[n]]$partext,
                value = model$par[[n]]$parval,
                step = model$par[[n]]$parval
            )
        })
        do.call(mainPanel, allp)
    })

    output$time <- renderUI({
        ntime = length(model$time)  #number of time variables in model
        allt = lapply(1:ntime, function(n) {
            numericInput(
                model$time[[n]]$timename,
                model$time[[n]]$timetext,
                value = model$time[[n]]$timeval,
                step = model$time[[n]]$timeval
            )
        })
        do.call(mainPanel, allt)
    })

    output$title <- renderUI({
        HTML(model$title)
    }) #creates title

}
