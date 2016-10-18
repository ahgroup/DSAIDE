## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  install.packages('DSAIDE')

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
library("DSAIDE")

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  dsaidemenu()

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  ?simulate_introduction

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
result <- simulate_introduction(S0 = 1000, I0 = 1, tmax = 300, g = 0.5,  b = 1/1000)

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
plot(result[,"time"],result[,"S"],xlab='Time',ylab='Number Susceptible',type='l')

## ---- eval=TRUE, echo=TRUE-----------------------------------------------
gvec = seq(0.1,2,by=0.1) #values of g which we went to run the simulation for
peak = rep(0,length(gvec)) #this will hold the peak values for each g
for (n in 1:length(gvec))
{
  result <- simulate_introduction(S0 = 1000, I0 = 1, tmax = 300, g = gvec[n],  b = 1/1000)
  peak[n] <- max(result[,"I"])
}
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

## ----eval=FALSE, echo=TRUE-----------------------------------------------
#  wvec = seq(0,2,by=0.1) #values of w which we went to run the simulation for
#  peak = rep(0,length(wvec)) #this will hold the peak values for each w
#  for (n in 1:length(wvec))
#  {
#    result <- mysimulator(S0 = 1000, I0 = 1, tmax = 300, g = 0.5,  b = 1/1000, w = wvec[n])
#    peak[n] <- max(result[,"I"])
#  }
#  plot(wvec,peak,type='p')

