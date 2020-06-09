############################################################
#This is a file for the Basic SIR App
#it contains additional information that helps properly process it
############################################################

appsettings = list()

#ID of app, unique for each app
appsettings$appid = 1

#Title of app, to be displayed on top of analyze tab
appsettings$apptitle = "Basic SIR Model"

#short version of app title. This needs to correspond to this and the Rmd file that goes with each app.
#this will be pulled by app.R to create action buttons
#it should contain app ID
appsettings$shorttitle = "1_basicsir"

#name of underlying simulation function(s) to be used in the app
#must be provided
appsettings$simfunction = c('simulate_sir_ode','simulate_sir_discrete')

#number of plots to produce for output
appsettings$nplots = 1

#specify the type of model that will be run
#if model type is provided as UI input, it should be set to NULL here
#otherwise it needs to be provided
appsettings$modeltype = "_mixed_"

