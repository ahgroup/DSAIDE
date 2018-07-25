############################################################
##code to simulate a basic SIR model as set of ODEs
##written by Andreas Handel (ahandel@uga.edu)
##last modified: 10/13/16
############################################################

# reproductive_number_ode_eq function
# This function is used in the solver function and has no independent usages
introductionode <- function(t, y, parms)
{
   with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
    {
  
      #the ordinary differential equations
  	  dS =  - b * S * I; #susceptibles
	  	dI = b * S * I - g * I; #infected/infectious
	 	  dR = g * I; #recovered

	 	  list(c(dS, dI, dR))
    }
   ) #close with statement
} #end function specifying the ODEs
	 	  

#' Simulation of a basic SIR model illustrating a single infectious disease outbreak
#'
#' @description This function runs a simulation of a basic SIR model
#' using a set of 3 ordinary differential equations.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' The function returns a matrix containing time-series of each variable and time.
#'
#' @param S0 initial number of susceptible individuals
#' @param I0 initial number of infected hosts
#' @param b level of infectiousness, i.e. rate of transmission of pathogen
#'   from infected to susceptible host
#' @param g rate at which a person leaves the infectious compartment, which
#'   is the inverse of the average duration of the infections period
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return The function returns the output from the odesolver as a matrix,
#' with one column per compartment/variable. The first column is time.
#' @details A simple SIR model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- simulate_introduction()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_introduction(S0 = 2000, I0 = 10, tmax = 100, g = 1, b = 1/100)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result$ts[ , "Time"], result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')
#' # Suppose we want to model the average duration of the infectious period as 4 days;
#' # the inverse of this is 0.25, which is the rate at which the person leaves
#' # the infectious stage.
#' result <- simulate_introduction(S0 = 2000, I0 = 10, tmax = 100, g = 0.25)
#' plot(result$ts[ , "Time"], result$ts[ , "S"], xlab = "Time", ylab = "Number Susceptible", type = "l")
#' # We could also set the rate of infectiousness very low.
#' result <- simulate_introduction(S0 = 2000, I0 = 10, tmax = 100, b = 0.0001)
#' plot(result$ts[ , "Time"], result$ts[ , "S"], xlab = "Time", ylab = "Number Susceptible", type = "l")
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Andreas Handel
#' @export

simulate_introduction <- function(S0 = 1000, I0 = 1, tmax = 300, g = 0.5, b = 1/1000)
{
  Y0 = c(S = S0, I = I0, R = 0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(b = b, g = g);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = introductionode, parms=pars, atol=1e-12, rtol=1e-12);
  
  colnames(odeoutput) <- c('Time','S','I','R')
  result <- list()
  result$ts <- as.data.frame(odeoutput)

  return(result)
}
