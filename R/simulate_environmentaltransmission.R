# This function is used in the solver function and has no independent usages
environmentaltransmissioneq <- function(t, y, parms)
{
  with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
    {
      
      #the ordinary differential equations
      dS =  - S * (beta1 * I + beta2* Iv + beta3 * E) + lambda1 - n1 * S; #susceptibles
      dI =  S * (beta1 * I + beta2* Iv + beta3 * E)  - n1 * I - gamma1 * I #infected, symptomatic
      dR =   gamma1 * I - n1 * R #recovered, immune

      dE = p * I - c * E; #pathogen in environment
    
      dSv = lambda2 - n2 * Sv - beta4 * I * Sv; #susceptible vectors
      dIv = beta4 * I * Sv - n2 * Iv - gamma2 * Iv; #susceptible hosts
            
      list(c(dS, dI, dR, dE, dSv, dIv))
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
#' @param Sv0 initial number of susceptible vectors
#' @param tmax maximum simulation time, units of months
#' @param beta1 rate of transmission from infected to susceptible host
#' @param beta2 rate of transmission from infected vector to susceptible host
#' @param beta3 rate of transmission from environment to susceptible host
#' @param beta4 rate of transmission from infected host to susceptible vector
#' @param lambda1 the rate of births of hosts
#' @param lambda2 the rate of births of vectors
#' @param n1 the rate of natural death of hosts
#' @param n2 the rate of natural death of vectors
#' @param gamma1 the rate at which infected hosts recover/die
#' @param gamma2 the rate at which infected vectors recover/die
#' @param p the rate at which infected host shed pathogen in the enviroment
#' @param c the rate at which pathogen in the enviroment decays
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
#'   result <- simulate_environmentaltransmission()
#'   # To choose parameter values other than the standard one, specify them e.g. like such
#'   result <- simulate_environmentaltransmission(S0 = 100, Sv0 = 1e5,  tmax = 100)
#'   # You should then use the simulation result returned from the function, e.g. like this:
#'   plot(result[,1],result[,2],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the shiny app 'EnvironmentalTransmission', which is part of this package, contains more details on the model
#' @author Andreas Handel
#' @references See e.g. the book "Modeling Infectious Diseases in Humans and Animals" by Keeling and Rohani 
#' for information on models of this type 
#' see the documentation for the deSolve package for details on ODE solvers
#' @export



simulate_environmentaltransmission <- function(S0 = 1e3, I0 = 1, Sv0 = 0, tmax = 120, beta1 = 0.01, beta2 = 0, beta3 = 0, beta4 = 0, lambda1 = 0, lambda2 = 0, n1 = 0, n2 = 0, gamma1 = 1, gamma2 = 1, p = 0 ,c = 0)
{
  ############################################################
  #setting initial conditions for variables
  #you can change initial conditions by changing the values assigned to 
  #S0, Sv0 and I0
  #every other compartment starts at 0
  Y0 = c(S = S0, I = I0, R = 0, E = 0, Sv = Sv0, Iv = 0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)
  
  
  ############################################################
  #vector of parameters which is sent to the ODE function  
  pars=c(beta1 = beta1, beta2 = beta2, beta3 = beta3, beta4 = beta4, lambda1 = lambda1, lambda2 = lambda2, n1 = n1, n2 = n2, gamma1 = gamma1, gamma2 = gamma2, p = p , c = c); 

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::lsoda(Y0, timevec, func = environmentaltransmissioneq, parms=pars, atol=1e-8, rtol=1e-8);

  return (odeoutput)
}
