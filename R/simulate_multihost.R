# This function is used in the solver function and has no independent usages
multieq <- function(t, y, parms)
{
  with(
    as.list(c(y, parms)), #lets us access variables and parameters stored in y and pars by name
    {
      
      dS <- -S*(b1*I1 + b2*I2 + b12*I12)
      dI1 <- (b1*I1 + a*b12*I12)*S - g1*I1 - b12*I1*I12
      dI2 <- (b2*I2 + (1 - a)*b12*I12)*S - g2*I2 - b12*I2*I12
      dI12 <- b12*I12*(I1 + I2 + R1 + R2) - g12*I12
      dR1 <- g1*I1 - b12*R1*I12
      dR2 <- g2*I2 - b12*R2*I12
      dR12 <- g12*I12
      
      
      list(c(dS, dI1, dI2, dI12, dR1, dR2, dR12))
    }
  ) #close with statement
} #end function specifying the ODEs

#' Simulation of a compartmental infectious disease transmission model with 2 types of hosts
#'
#' @description  This model allows for the simulation of an ID with 2 types of hosts
#' 
#'
#' @param S initial total number of hosts 
#' @param I1 initial number of hosts infected with type 1
#' @param I2 initial number of hosts infected with type 2
#' @param tmax maximum simulation time, units of months
#' @param b1 rate of transmission of type 1 pathogen to host
#' @param b2 rate of transmission of type 2 pathogen to host
#' @param b12 rate of transmission from infected type 2 host to susceptible type 1 host
#' @param g1 the rate at which infected type 1 hosts recover
#' @param g2 the rate at which infected type 2 hosts recover
#' @param g12 the rate at which hosts infected with type 1 and 2 recover
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#' @section Warning:
#'   This function does not perform any error checking. So if you try to do
#'   something nonsensical (e.g. any negative values or fractions > 1),
#'   the code will likely abort with an error message
#' @examples
#'   # To run the simulation with default parameters just call this function
#'   result <- simulate_heterogeneity()
#'   # To choose parameter values other than the standard one, specify them e.g. like such
#'   result <- simulate_heterogeneity(S10 = 100, S20 = 1e3,  tmax = 100)
#'   # You should then use the simulation result returned from the function, e.g. like this:
#'   plot(result[,1],result[,2],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the shiny app 'Host Heterogeneity', which is part of this package, contains more details on the model
#' @author Andreas Handel
#' @references See e.g. Keeling and Rohani 2008 for SIR models and the
#'   documentation for the deSolve package for details on ODE solvers
#' @export


simulate_multihost <- function(S = 1e3, I10 = 1, I20 = 0, I12 = 0, R1 = 0, R2 = 0, R12 = 0, tmax = 120, a = 0, b1 = 0, b2 = 0, b12 = 0, g1 = 1, g2 = 1, g12 = 1)
{
  ############################################################
  Y0 = c(S = S, I1 = I10, I2 = I20, I12 = I12, R1 = R1, R2 = R2, R12 = R12);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)
  
  
  ############################################################
  #vector of parameters which is sent to the ODE function  
  pars <- c(a = a, b1 = b1, b2 = b2, b12 = b12, g1 = g1, g2 = g2, g12 = g12);
  
  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::lsoda(Y0, timevec, func = multieq, parms=pars, atol=1e-12, rtol=1e-12);
  
  return (odeoutput)
}
