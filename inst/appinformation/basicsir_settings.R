############################################################
#This is a file for the Basic SIR App
#it contains additional information that helps properly process it
############################################################

appsettings = list()

#Title of app, to be displayed on top of analyze tab
appsettings$apptitle = "Basic SIR Model"

#name of underlying simulation function(s) to be used in the app
#must be provided
appsettings$simfunction = c('simulate_sir_ode','simulate_sir_discrete')

#number of plots to produce for output
appsettings$nplots = 1

#specify the type of model that will be run
#if model type is provided as UI input, it should be set to NULL here
#otherwise it needs to be provided
appsettings$modeltype = NULL

#additional input elements for app that are shown on UI
appsettings$otherinputs =   list(
  shiny::selectInput("modeltype", "Models to run",c("ODE" = '_ode_', 'discrete' = '_discrete_', 'both' = '_ode_and_discrete_'), selected = '_ode_'),
  shiny::selectInput("plotscale", "Log-scale for plot",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both")),
  shiny::selectInput("plotengine", "Plot engine",c("ggplot" = "ggplot", "plotly" = "plotly"))
) #end list
