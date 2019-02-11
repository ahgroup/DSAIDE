#' Vector transmission model
#' 
#' A basic model with several compartments to model vector-borne transmission
#' 
#' @details The model tracks the dynamics of susceptible, infected, and recovered hosts, and susceptible and infected vectors. Infection, recovery, and waning immunity processes are implemented for hosts. Births and deaths and infection processes are implemented for vectors.
#' This code is based on a dynamical systems model created by the modelbuilder package.  
#' The model is implemented here as a set of ordinary differential equations, 
#' using the deSolve package. 
#' @param Sh : starting value for Susceptible hosts : numeric
#' @param Ih : starting value for Infected hosts : numeric
#' @param Rh : starting value for Recovered hosts : numeric
#' @param Sv : starting value for Susceptible Vectors : numeric
#' @param Iv : starting value for Infected Vectors : numeric
#' @param b1 : infection rate of hosts : numeric
#' @param b2 : infection rate of vectors : numeric
#' @param g : recovery rate of hosts : numeric
#' @param w : wanning immunity rate : numeric
#' @param m : vector birth rate : numeric
#' @param n : vector death rate : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : Time step : numeric
#' @return The function returns the output as a list. 
#' The time-series from the simulation is returned as a dataframe saved as list element \code{ts}. 
#' The \code{ts} dataframe has one column per compartment/variable. The first column is time.   
#' @examples  
#' # To run the simulation with default parameters:  
#' result <- simulate_vectortransmission_ode() 
#' @section Warning: This function does not perform any error checking. So if you try to do something nonsensical (e.g. have negative values for parameters), the code will likely abort with an error message.
#' @export 
 
simulate_vectortransmission_ode <- function(Sh = 1000, Ih = 1, Rh = 0, Sv = 1000, Iv = 1, b1 = 0.002, b2 = 0.002, g = 1, w = 0.1, m = 100, n = 0.1, tstart = 0, tfinal = 100, dt = 0.1 ) 
{ 
  #Block of ODE equations for deSolve 
  Vector_transmission_model_ode <- function(t, y, parms) 
  {
    with( as.list(c(y,parms)), { #lets us access variables and parameters stored in y and parms by name 
    #StartODES
    #Susceptible hosts : infection of susceptible hosts : waning immunity :
    dSh = -b1*Sh*Iv +w*Rh
    #Infected hosts : infection of susceptible hosts : recovery of infected :
    dIh = +b1*Sh*Iv -g*Ih
    #Recovered hosts : recovery of infected hosts : waning immunity :
    dRh = +g*Ih -w*Rh
    #Susceptible Vectors : vector births : infection of susceptible vectors : death of susceptible vectors :
    dSv = +m -b2*Sv*Ih -n*Sv
    #Infected Vectors : infection of susceptible vectors : death of infected vectors :
    dIv = +b2*Sv*Ih -n*Iv
    #EndODES
    list(c(dSh,dIh,dRh,dSv,dIv)) 
  } ) } #close with statement, end ODE code block 
 
  #Main function code block 
  timevec=seq(tstart, tfinal, by = dt) 
  vars = c(Sh = Sh, Ih = Ih, Rh = Rh, Sv = Sv, Iv = Iv)
  pars = c(b1 = b1, b2 = b2, g = g, w = w, m = m, n = n)
  
  odeout = deSolve::ode(y = vars, parms = pars, times = timevec,  func = Vector_transmission_model_ode) 
  result <- list() 
  result$ts <- as.data.frame(odeout) 
  return(result) 
} 
