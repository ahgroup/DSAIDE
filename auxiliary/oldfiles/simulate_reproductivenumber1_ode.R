#' Basic SIR model
#' 
#' @description A basic SIR model with 3 compartments and infection and recovery processes
#' 
#' @details The model includes susceptible, infected, and recovered compartments. 
#' The two processes that are modeled are infection and recovery.
#' The simulation also monitors the number of infected and when they drop below 1, they are set to 0.
#' @param S : starting value for Susceptible : numeric
#' @param I : starting value for Infected : numeric
#' @param R : starting value for Recovered : numeric
#' @param b : infection rate : numeric
#' @param g : recovery rate : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : Time step : numeric
#' @return The function returns the output as a list. 
#' The time-series from the simulation is returned as a dataframe saved as list element \code{ts}. 
#' The \code{ts} dataframe has one column per compartment/variable. The first column is time.   
#' @examples  
#' # To run the simulation with default parameters:  
#' result <- simulate_reproductivenumber1_ode() 
#' @section Warning: This function does not perform any error checking. So if you try to do something nonsensical (e.g. have negative values for parameters), the code will likely abort with an error message.
#' @export 
 
simulate_reproductivenumber1_ode <- function(S = 1000, I = 1, R = 0, b = 0.002, g = 1, tstart = 0, tfinal = 100, dt = 0.1) 
{ 
  #Block of ODE equations for deSolve 
  reproductive1_ode <- function(t, y, parms) 
  {
    with( as.list(c(y,parms)), { #lets us access variables and parameters stored in y and parms by name 
    #Susceptible : infection of susceptibles :
    dS = -b*S*I
    #Infected : infection of susceptibles : recovery of infected :
    dI = +b*S*I -g*I
    #Recovered : recovery of infected :
    dR = +g*I
    list(c(dS,dI,dR)) 
  } ) } #close with statement, end ODE code block 
 
  ############################################################
  # functions that monitor I and sets it to 0 if it crosses the 1 threshold 
  checkinfected <- function(t,y,parms)
  {
    y["I"]-0.99
  }
  zeroinfected <- function(t,y,parms)
  {
    y["I"] = 0
    return(y)
  }
  
  
  #Main function code block 
  Y0 = c(S = S, I = I, R = R)
  pars = c(b = b, g = g)
  timevec=seq(tstart,tfinal,by=dt) 
  odeout = deSolve::ode(y = Y0, times = timevec, func = reproductive1_ode, parms=pars, method = "lsoda", events = list(func = zeroinfected, root = TRUE), rootfun = checkinfected, atol=1e-8, rtol=1e-8);
  result <- list() 
  result$ts <- as.data.frame(odeout) 
  return(result) 
} 
