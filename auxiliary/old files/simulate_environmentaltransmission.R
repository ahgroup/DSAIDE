#Code to simulate an SIR model with an environmental transmission stage

# This function is used in the solver function and has no independent usages
environmentaltransmissioneq <- function(t, y, parms)
{
  with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
    {
      
      #the ordinary differential equations
      dS = m  - S * (bd * I + be *  E)  - n * S; #susceptibles
      dI =  S * (bd * I + be * E)  - n * I - g * I #infected, symptomatic
      dR =   g * I - n * R #recovered, immune
      dE = p * I - c * E; #pathogen in environment

      list(c(dS, dI, dR, dE))
    }
  ) #close with statement
} #end function specifying the ODEs


#' Simulation of a compartmental infectious disease transmission model illustrating environmental transmission
#'
#' @description  This model allows for the simulation of an environmentally transmitted infectious disease
#' 
#'
#' @param S0 initial number of susceptible hosts
#' @param I0 initial number of infected hosts
#' @param E0 initial number of pathogen in environment
#' @param tmax maximum simulation time, units of months
#' @param bd rate of direct transmission 
#' @param be rate of environmental transmission 
#' @param m rate of births of hosts
#' @param n the rate of natural death of hosts
#' @param g the rate at which infected hosts recover/die
#' @param p the rate at which infected host shed pathogen in the environment
#' @param c the rate at which pathogen in the environment decays
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver.
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#' @section Warning:
#'   This function does not perform any error checking. So if you try to do
#'   something nonsensical (e.g. any negative values or fractions > 1),
#'   the code will likely abort with an error message.
#' @examples
#'   # To run the simulation with default parameters just call the function:
#'   result <- simulate_environmentaltransmission()
#'   # To choose parameter values other than the standard one, specify them like such:
#'   result <- simulate_environmentaltransmission(S0 = 100, E0 = 1e5, tmax = 100)
#'   # You should then use the simulation result returned from the function, like this:
#'   plot(result$ts[,"Time"],result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')
#'   # Consider also a case in which we set the birth rate of hosts at 0.2, and
#'   # the rate at which infected hosts recover or die at 0.8.
#'   result <- simulate_environmentaltransmission(S0 = 1000, E0 = 1e5, m = 0.2, g = 0.8)
#'   plot(result$ts[,"Time"],result$ts[,"S"], xlab="Time", ylab = "Number Susceptible",type="l")
#'   # Additionally, consider a case in which we assume that no new hosts are born.
#'   result <- simulate_environmentaltransmission(S0 = 1000, E0 = 1e5, m = 0)
#'   plot(result$ts[,"Time"], result$ts[,"S"], xlab = "Time", ylab = "Number Susceptible")
#' @seealso The UI of the Shiny app 'EnvironmentalTransmission', which is part of this package, contains more details on the model.
#' @author Andreas Handel
#' @references See e.g. the book "Modeling Infectious Diseases in Humans and Animals" by Keeling and Rohani 
#' for information on models of this type 
#' see the documentation for the deSolve package for details on ODE solvers
#' @export



simulate_environmentaltransmission <- function(S0 = 1e3, I0 = 1, E0 = 0, tmax = 120, bd = 0.01, be = 0,  m = 0, n = 0, g = 1, p = 0 ,c = 0)
{
  ############################################################
  Y0 = c(S = S0, I = I0, R = 0, E = E0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  ############################################################
  #vector of parameters which is sent to the ODE function  
  pars=c(bd = bd, be = be, m = m, n = n, g = g, p = p, c = c); 

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the remaining columns the values for the variables
  #returned in the order as specified in Y0 and the return from the solver function
  odeoutput = deSolve::lsoda(Y0, timevec, func = environmentaltransmissioneq, parms=pars, atol=1e-12, rtol=1e-12);
  
  colnames(odeoutput) <- c("Time", "S", "I", "R", "E")

  #return result as list, with element ts containing the time-series
  result <- list()
  result$ts <- as.data.frame(odeoutput)

  return(result)
}
