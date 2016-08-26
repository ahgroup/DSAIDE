############################################################
##simulating a basic SIR model
##written by Andreas Handel, ahandel@uga.edu, last change: 8/2/16
############################################################

# reproductive_number_ode_eq function
# This function is used in the solver function and has no independent usages
introductionode <- function(t, y, pars)
{
      #assigning the y/variable vector to each of the compartments - susceptibles (S), infecteds (I), recovereds (R)
      S = y[1];
      I = y[2];
      R = y[3];

      #assigning the parameter vector to each of the parameter values
      #level of infectiousness/rate of transmission (beta), duration of infectious period (1/gamma)
      beta = pars[1];
      gamma = pars[2];

      #the ordinary differential equations
  	  dS =  - beta * S * I; #susceptibles
	  	dI = beta * S * I - gamma * I; #infected/infectious
	 	  dR = gamma * I; #recovered

      return(list(c(dS, dI, dR)));
} #end function specifying the ODEs

#' Simulator for a basic SIR model
#'
#' @description This function runs a simulation of a basic SIR model
#' using a set of 3 ordinary differential equations.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' The function returns a matrix containing time-series of each variable and time.
#'
#' @param PopSize specifies the initial number of individuals (suceptibles +
#'   infected)
#' @param I0 initial number of infected hosts
#' @param beta level of infectiousness, i.e. rate of transmission of pathogen
#'   from infected to susceptible host
#' @param gamma rate at which a person leaves the infectious compartment, which
#'   is the inverse of the average duration of the infections period
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return The function returns the output from the odesolver as a matrix,
#' with one column per compartment/variable. The first column is time.
#' @details A simple SIR model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.#'
#' @section Warning:
#' This function does not perform any error checking. So if you try to do
#' something nonsensical (e.g. have I0 > PopSize or any negative values), the
#' code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- simulate_introduction()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_introduction(PopSize = 2000, I0 = 10, tmax = 100, gamma = 1, beta = 1/100)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result[,1],result[,2],xlab='Time',ylab='Number Susceptible',type='l')
#' @references See e.g. Keeling and Rohani 2008 for SIR models and the
#'   documentation for the deSolve package for details on ODE solvers
#' @author Andreas Handel
#' @export

simulate_introduction <- function(PopSize = 1000, I0 = 1, tmax = 300, gamma = 0.5, beta = 1/1000){

  S0 = PopSize - I0; #initial number of uninfected hosts
  Y0 = c(S = S0, I = I0, R = 0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(beta, gamma);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = introductionode, parms=pars, atol=1e-12, rtol=1e-12);

  #The output produced by a call to the odesolver is odeoutput matrix is returned by the function
  return(odeoutput)
}
