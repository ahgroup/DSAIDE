## ----apptable, echo=FALSE------------------------------------------------
appnames=c('ID Dynamics Intro','Characteristics of ID','ID Patterns','Direct Transmission','Environmental Transmission','Vector Transmission','Reproductive Number','ID Control','Host Heterogeneity','Stochastic dynamics','Evolutionary dynamics','Multi-Pathogen Dynamics')

appmodel=c('3 compartment (SIR) ODE model','6 compartment ODE model','6 compartment ODE model. Includes births-deaths, waning immunity, seasonality. ','3 compartment ODE model. Births, deaths and waning immunity are included.','4 compartment ODE model. Includes explicit environmental stage. Births-deaths and waning immunity are included.','5 compartment ODE model. Includes susceptible and infected vectors and their dynamics. Births and deaths for vectors and waning immunity for hosts are included.','3 compartment ODE model.  Includes vaccination of population at the beginning of the simulation. Births, deaths and waning immunity are included.','9 compartment ODE model. An environmental and 2 vector stages as well as 6 host stages.','6 compartment ODE model. 2x SIR for 2 different hosts.','4 compartment (SEIR) stochastic model.','5 compartment stochastic model. Untreated and treated hosts infected with wild-type, and hosts infected with a resistant strain.','9 compartment ODE model with basic SIR dynamics.')

apptopic=c('A first introduction to a simple compartmental SIR model. Allows simulation of a single outbreak for different parameter and initial condition settings.','The potential role of different disease states (e.g. pre-symptomatic, asymptomatic, symptomatic) on ID dynamics.','Different ID patterns (single outbreak, osciallations, steady states).','The differences between density-dependent and frequency-dependent transmission and their impact on ID dynamics.','The impact of environmental shedding, decay and transmission.','Exploration of a simple vector-borne transmission model.','The reproductive number concept and how to estimate it from (simulated) data.','The impact of different control measures for different types of ID.','The impact of host heterogeneity and core groups on ID dynamics and control.','The impact of stochasticity on dynamics, the phenomenon of ID extinction.','Interaction between drug treatment and evolution/emergence of drug resistance.',' Infection dynamics with 2 pathogens that act independently.')

apptable = data.frame(App_Name = appnames, Model = appmodel, Topic_covered = apptopic)
knitr::kable(apptable)

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  install.packages('DSAIDE')

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  install.packages('devtools')
#  library('devtools')
#  install_github("ahgroup/DSAIDE")

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  library('DSAIDE')

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  dsaidemenu()

## ---- eval=TRUE, echo=FALSE, message=FALSE-------------------------------
library('DSAIDE')

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  help('simulate_introduction')

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
result <- simulate_introduction(S0 = 500, I0 = 5, tmax = 100, g = 0.1,  b = 1/2500)

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
plot(result[,"time"],result[,"S"],xlab='Time',ylab='Number Susceptible',type='l')

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
gvec = seq(0.01,0.3,by=0.01) #values of recovery rate, g, for which to run the simulation 
peak = rep(0,length(gvec)) #this will record the peak values for each g
for (n in 1:length(gvec))
{
  #call the simulator function with different values of g each time
  result <- simulate_introduction(S0 = 500, I0 = 5, tmax = 200, g = gvec[n],  b = 1/2500)
  peak[n] <- max(result[,"I"]) #record max number of infected for each value of g
}
#plot final result
plot(gvec,peak,type='p',xlab='Rate of recovery',ylab='Max number of infected')

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  simulate_introduction <- function(S0 = 1000, I0 = 1, tmax = 300, g = 0.5, b = 1/1000)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  mysimulator <- function(S0 = 1000, I0 = 1, tmax = 300, g = 0.5, b = 1/1000, w = 0)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  pars = c(b = b, g = g);

## ----eval=FALSE, echo=TRUE, color='red'----------------------------------
#  pars = c(b = b, g = g, w = w);

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  dS =  - b * S * I; #susceptibles
#  dI = b * S * I - g * I; #infected/infectious
#  dR = g * I; #recovered

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  dS =  - b * S * I + w * R; #susceptibles
#  dI = b * S * I - g * I; #infected/infectious
#  dR = g * I -  w * R; #recovered

## ----eval=TRUE, echo=TRUE------------------------------------------------
source('mysimulator.R') #to initialize the new function - it needs to be in same directory as this code
wvec = seq(0,1,by=0.02) #values of immunity loss rate, w, for which to run the simulation 
peak = rep(0,length(wvec)) #this will record the peak values for each g
for (n in 1:length(wvec))
{
  result <- mysimulator(S0 = 1000, I0 = 1, tmax = 300, g = 0.5,  b = 1/1000, w = wvec[n])
  peak[n] <- max(result[,"I"])
}
plot(wvec,peak,type='p',xlab='Rate of waning immunity',ylab='Max number of infected')

