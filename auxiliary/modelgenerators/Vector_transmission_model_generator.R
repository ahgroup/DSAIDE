#this is a proposed list structure for models
#a vector transmission model is implemented
#this structure should be created by the shiny app, as needed saved as Rdata file
#and read by various functions and turned into desolve/adaptivetau/discrete time/RxODE code


#main list structure
mbmodel = list()

#some meta-information
mbmodel$title = "Vector transmission model"
mbmodel$description = "A basic model with several compartments to model vector-borne transmission"
mbmodel$author = "Andreas Handel"
mbmodel$date = Sys.Date()
mbmodel$details = 'The model tracks the dynamics of susceptible, infected, and recovered hosts, and susceptible and infected vectors. Infection, recovery, and waning immunity processes are implemented for hosts. Births and deaths and infection processes are implemented for vectors.'

#list of elements for each model variable.
var = vector("list",5)
var[[1]]$varname = "Sh"
var[[1]]$vartext = "Susceptible hosts"
var[[1]]$varval = 1000
var[[1]]$flows = c('-b1*Sh*Iv','+w*Rh')
var[[1]]$flownames = c('infection of susceptible hosts','waning immunity')

var[[2]]$varname = "Ih"
var[[2]]$vartext = "Infected hosts"
var[[2]]$varval = 1
var[[2]]$flows = c('+b1*Sh*Iv','-g*Ih')
var[[2]]$flownames = c('infection of susceptible hosts','recovery of infected')

var[[3]]$varname = "Rh"
var[[3]]$vartext = "Recovered hosts"
var[[3]]$varval = 0
var[[3]]$flows = c('+g*Ih','-w*Rh')
var[[3]]$flownames = c('recovery of infected hosts','waning immunity')

var[[4]]$varname = "Sv"
var[[4]]$vartext = "Susceptible Vectors"
var[[4]]$varval = 1000
var[[4]]$flows = c('+b','-b2*Sv*Ih','+n*Sv')
var[[4]]$flownames = c('vector births','infection of susceptible vectors','death of susceptible vectors')

var[[5]]$varname = "Iv"
var[[5]]$vartext = "Infected Vectors"
var[[5]]$varval = 1
var[[5]]$flows = c('+b2*Sv*Ih','-n*Iv')
var[[5]]$flownames = c('infection of susceptible vectors', 'death of infected vectors')

mbmodel$var = var


#list of elements for each model parameter.
par = vector("list",6)
par[[1]]$parname = c('b1')
par[[1]]$partext = 'infection rate of hosts'
par[[1]]$parval = 2e-3

par[[2]]$parname = c('b2')
par[[2]]$partext = 'infection rate of vectors'
par[[2]]$parval = 2e-3

par[[3]]$parname = c('g')
par[[3]]$partext = 'recovery rate of hosts'
par[[3]]$parval = 1

par[[4]]$parname = c('w')
par[[4]]$partext = 'wanning immunity rate'
par[[4]]$parval = 0.1

par[[5]]$parname = c('b')
par[[5]]$partext = 'vector birth rate'
par[[5]]$parval = 100

par[[6]]$parname = c('n')
par[[6]]$partext = 'vector death rate'
par[[6]]$parval = 0.1

mbmodel$par = par

#time parvals
time = vector("list",3)
time[[1]]$timename = "tstart"
time[[1]]$timetext = "Start time of simulation"
time[[1]]$timeval = 0

time[[2]]$timename = "tfinal"
time[[2]]$timetext = "Final time of simulation"
time[[2]]$timeval = 100

time[[3]]$timename = "dt"
time[[3]]$timetext = "Time step"
time[[3]]$timeval = 0.1

mbmodel$time = time

modelname = gsub(" ","_",mbmodel$title)
rdatafile = paste0(modelname,'.rds')
saveRDS(mbmodel,file = rdatafile)

