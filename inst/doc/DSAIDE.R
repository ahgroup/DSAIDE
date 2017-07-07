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

