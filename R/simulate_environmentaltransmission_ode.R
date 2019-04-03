#' Environmental Transmission model
#' 
#' @description An SIR model including environmental transmission
#' 
#' @details The model includes susceptible, infected, recovered and environmental compartments. 
#' @param S : starting value for Susceptible : numeric
#' @param I : starting value for Infected : numeric
#' @param R : starting value for Recovered : numeric
#' @param E : starting value for Environmental Pathogen : numeric
#' @param bd : direct transmission rate : numeric
#' @param be : environmental transmission rate : numeric
#' @param m : births : numeric
#' @param n : natural deaths : numeric
#' @param g : recovery rate : numeric
#' @param p : shedding rate : numeric
#' @param c : decay rate : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : Time step : numeric
#' @return The function returns the output as a list. 
#' The time-series from the simulation is returned as a dataframe saved as list element \code{ts}. 
#' The \code{ts} dataframe has one column per compartment/variable. The first column is time.   
#' @examples  
#' # To run the simulation with default parameters:  
#' result <- simulate_environmentaltransmission_ode() 
#' @section Warning: This function does not perform any error checking. So if you try to do something nonsensical (e.g. have negative values for parameters), the code will likely abort with an error message.
#' @export 
 
simulate_environmentaltransmission_ode <- function(S = 10000, I = 1, R = 0, E = 0, bd = 1e-04, be = 0, m = 0, n = 0, g = 0.2, p = 0, c = 10, tstart = 0, tfinal = 100, dt = 0.1 ) 
{ 
  #Block of ODE equations for deSolve 
  Environmental_Transmission_model_ode <- function(t, y, parms) 
  {
    with( as.list(c(y,parms)), { #lets us access variables and parameters stored in y and parms by name 
    #StartODES
    #Susceptible : births : natural death : direct infection : environmental infection :
    dS = +m -n*S -bd*I*S -be*E*S
    #Infected : direct infection : environmental infection : natural death : recovery of infected :
    dI = +bd*I*S +be*E*S -n*I -g*I
    #Recovered : natural death : recovery of infected :
    dR =  g*I - n*I
    #Environmental Pathogen : shedding by infected : decay :
    dE = +p*I - c*E
    #EndODES
    list(c(dS,dI,dR,dE)) 
  } ) } #close with statement, end ODE code block 
 
  #Main function code block 
  timevec=seq(tstart,tfinal, by = dt) 
  vars = c(S = S, I = I, R = R, E = E)
  pars = c(bd = bd, be = be, m = m, n = n, g = g, p = p, c = c)
  odeout = deSolve::ode(y = vars, parms = pars, times = timevec,  func = Environmental_Transmission_model_ode) 
  result <- list() 
  result$ts <- as.data.frame(odeout) 
  return(result) 
} 
