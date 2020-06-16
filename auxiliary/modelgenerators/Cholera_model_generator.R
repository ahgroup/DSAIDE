#this is a proposed list structure for models
#a model based on Codeco 2001 BMC ID is implemented
#this structure should be created by the shiny app, as needed saved as Rdata file
#and read by various functions and turned into desolve/adaptivetau/discrete time/RxODE code


#main list structure
mbmodel = list()

#some meta-information
mbmodel$title = "Cholera model"
mbmodel$description = "A Cholera model based on Codeco 2001 BMC ID"
mbmodel$author = "Andreas Handel"
mbmodel$date = Sys.Date()
mbmodel$details = 'The model includes susceptible, infected, and environmental compartments. See reference for more details.'


var = vector("list",3)
var[[1]]$varname = "S"
var[[1]]$vartext = "Susceptible"
var[[1]]$varval = 10000
var[[1]]$flows = c('+n*h','-n*S','-a*B/(k+B)*S')
var[[1]]$flownames = c('births','deaths','infection of susceptibles')

var[[2]]$varname = "I"
var[[2]]$vartext = "Infected"
var[[2]]$varval = 1
var[[2]]$flows = c('+a*B/(k+B)*S','-r*I')
var[[2]]$flownames = c('infection of susceptibles','recovery of infected')

var[[3]]$varname = "B"
var[[3]]$vartext = "Bacteria"
var[[3]]$varval = 0
var[[3]]$flows = c('+nb*B', '-mb*B', '+e*I')
var[[3]]$flownames = c('bacteria growth','bacteria decay','bacteria shedding')

mbmodel$var = var

#list of elements for each model parameter.
par = vector("list",8)
par[[1]]$parname = 'h'
par[[1]]$partext = 'population size'
par[[1]]$parval = 10000

par[[2]]$parname = 'n'
par[[2]]$partext = 'birth rate'
par[[2]]$parval = 0.0001

par[[3]]$parname = 'a'
par[[3]]$partext = 'infection rate'
par[[3]]$parval =   1

par[[4]]$parname = 'k'
par[[4]]$partext = '50% infection rate'
par[[4]]$parval =  1e6

par[[5]]$parname = 'r'
par[[5]]$partext = 'recovery rate'
par[[5]]$parval =  0.2

par[[6]]$parname = 'nb'
par[[6]]$partext = 'bacteria growth rate'
par[[6]]$parval =  1

par[[7]]$parname = 'mb'
par[[7]]$partext = 'bacteria death rate'
par[[7]]$parval =  1.33

par[[8]]$parname = 'e'
par[[8]]$partext = 'bacteria shedding rate'
par[[8]]$parval =  10

mbmodel$par = par

#time parvals
time = vector("list",3)
time[[1]]$timename = "tstart"
time[[1]]$timetext = "Start time of simulation"
time[[1]]$timeval = 0

time[[2]]$timename = "tfinal"
time[[2]]$timetext = "Final time of simulation"
time[[2]]$timeval = 500

time[[3]]$timename = "dt"
time[[3]]$timetext = "Time step"
time[[3]]$timeval = 0.1

mbmodel$time = time

modelname = gsub(" ","_",mbmodel$title)
rdatafile = paste0(modelname,'.rds')
saveRDS(mbmodel,file = rdatafile)

