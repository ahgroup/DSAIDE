## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  library('DSAIDE')

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  dsaidemenu()

## ----eval=TRUE, echo=FALSE-----------------------------------------------
library('DSAIDE') 

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  help('simulate_sir_ode')

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
result <- simulate_sir_ode()

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
plot(result$ts[ , "time"],result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
result <- simulate_sir_ode(S = 2000, b = 0.001, g = 0.5, tfinal = 200)
plot(result$ts[ , "time"],result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
gvec = seq(0.01,0.3,by=0.01) #values of recovery rate, g, for which to run the simulation 
peak = rep(0,length(gvec)) #this will record the peak values for each g
for (n in 1:length(gvec))
{
  #call the simulator function with different values of g each time
  result <- simulate_sir_ode(S = 500, b = 1/2500, g = gvec[n],  tfinal = 200)
  peak[n] <- max(result$ts[,"I"]) #record max number of infected for each value of g
}
#plot final result
plot(gvec,peak,type='p',xlab='Rate of recovery',ylab='Max number of infected')

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  simulate_sir_ode <- function(S = 1000, I = 1, R = 0, b = 0.002, g = 1, tstart = 0, tfinal = 100, dt = 0.1 )

## ----eval=FALSE, echo=TRUE, color='red'----------------------------------
#  mysimulator <- function( S = 1000, I = 1, R = 0, b = 0.002, g = 1, w = 0, tstart = 0, tfinal = 100, dt = 0.1 )

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  pars = c(b = b, g = g)

## ----eval=FALSE, echo=TRUE, color='red'----------------------------------
#  pars = c(b = b, g = g, w = w)

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  dS = -b*S*I
#  dI = b*S*I -g*I
#  dR = g*I

## ----eval=FALSE, echo=TRUE, color='red'----------------------------------
#  dS = -b*S*I +w*R
#  dI = b*S*I -g*I
#  dR = g*I -w*R

## ----eval=TRUE, echo=TRUE------------------------------------------------
source('mysimulator.R') #to initialize the new function - it needs to be in same directory as this code
wvec = seq(0,1,by=0.02) #values of immunity loss rate, w, for which to run the simulation 
peak = rep(0,length(wvec)) #this will record the peak values for each g
for (n in 1:length(wvec))
{
  result <- mysimulator( S = 1000, I = 1, R = 0, b = 1e-3, g = 0.5, w = wvec[n], tstart = 0, tfinal = 300, dt = 0.1)
  peak[n] <- max(result$ts[,"I"])
}
plot(wvec,peak,type='p',xlab='Rate of waning immunity',ylab='Max number of infected')

