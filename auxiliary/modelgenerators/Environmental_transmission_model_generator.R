#this is a model encoded in the form used by the modelbuilder package
#this structure can be created graphically by the modelbuilder package or by hand
#this model is an SIR model with environmental transmission, created by hand


#main list structure
mbmodel = list()

#some meta-information
mbmodel$title = "Environmental Transmission model"
mbmodel$description = "An SIR model including environmental transmission"
mbmodel$author = "Andreas Handel"
mbmodel$date = Sys.Date()
mbmodel$details = 'The model includes susceptible, infected, and environmental compartments. This model is part of the DSAIDE package and explained in that package in detail.'


var = vector("list",4)
var[[1]]$varname = "S"
var[[1]]$vartext = "Susceptible"
var[[1]]$varval = 10000
var[[1]]$flows = c('+m','-n*S','-bd*I*S','-be*E*S')
var[[1]]$flownames = c('births','natural death','direct infection','environmental infection')

var[[2]]$varname = "I"
var[[2]]$vartext = "Infected"
var[[2]]$varval = 1
var[[2]]$flows = c('+bd*I*S','+be*E*S','-n*I','-g*I')
var[[2]]$flownames = c('direct infection','environmental infection','natural death','recovery of infected')

var[[3]]$varname = "R"
var[[3]]$vartext = "Recovered"
var[[3]]$varval = 0
var[[3]]$flows = c('-n*I','+g*I')
var[[3]]$flownames = c('natural death', 'recovery of infected')

var[[4]]$varname = "E"
var[[4]]$vartext = "Environmental Pathogen"
var[[4]]$varval = 0
var[[4]]$flows = c('+p*I','+c*E')
var[[4]]$flownames = c('shedding by infected','decay')

mbmodel$var = var

#list of elements for each model parameter.
par = vector("list",7)
par[[1]]$parname = 'bd'
par[[1]]$partext = 'direct transmission rate'
par[[1]]$parval = 1e-4

par[[2]]$parname = 'be'
par[[2]]$partext = 'environmental transmission rate'
par[[2]]$parval = 0

par[[3]]$parname = 'm'
par[[3]]$partext = 'birth rate'
par[[3]]$parval =   0

par[[4]]$parname = 'n'
par[[4]]$partext = 'natural death rate'
par[[4]]$parval =  0

par[[5]]$parname = 'g'
par[[5]]$partext = 'recovery rate'
par[[5]]$parval =  0.2

par[[6]]$parname = 'p'
par[[6]]$partext = 'rate at which infected host shed pathogen into the environment'
par[[6]]$parval = 0

par[[7]]$parname = 'c'
par[[7]]$partext = 'rate at which pathogen in the environment decays'
par[[7]]$parval =  10

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

