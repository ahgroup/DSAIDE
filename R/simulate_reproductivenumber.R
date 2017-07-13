
# reproductive_number_ode_eq function
# This function is used in the solver function and has no independent usages
reproductivenumberode <- function(t, y, parms)
{
  with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
    {    
  
      #the ordinary differential equations
  	  dS = m - n*S - b*S*I +  w*R; #susceptibles
	  	dI = b*S*I - g*I - n*I; #infected/infectious
	 	  dR = g*I - n*R - w*R; #recovered
	 	  
	 	  list(c(dS, dI, dR))
	 	  
    }
  ) #close with statement
} #end function specifying the ODEs
	 	  
 	  

#' Simulation of a compartmental infectious disease transmission model to study the reproductive number
#' 
#' @description  Simulation of a basic SIR compartmental model with these compartments:
#'   Susceptibles (S), Infected/Infectious (I),
#'   Recovered and Immune (R).
#'   
#'   The model is assumed to be in units of months when run through the Shiny App.
#'   However as long as all parameters are chosen in the same units, 
#'   one can directly call the simulator assuming any time unit.
#'
#' @param S0 initial number of susceptible hosts
#' @param I0 initial number of infected hosts
#' @param f fraction of vaccinated individuals. Those individuals are moved from S to R at the beginning of the simulation
#' @param e efficay of vaccine, given as fraction between 0 and 1
#' @param b level/rate of infectiousness for hosts in the I compartment
#' @param g rate at which a person leaves the I compartment
#' @param w rate at which recovered persons loose immunity and return to susceptible state
#' @param m the rate at which new individuals enter the model (are born)
#' @param n the rate of natural death (the inverse it the average lifespan)
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver.
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#' @section Warning:
#'   This function does not perform any error checking. So if you try to do
#'   something nonsensical (e.g. negative values or fractions > 1),
#'   the code will likely abort with an error message.
#' @examples
#'   # To run the simulation with default parameters just call this function
#'   result <- simulate_reproductivenumber()
#'   # To choose parameter values other than the standard one, specify them e.g. like such
#'   result <- simulate_reproductivenumber(S0 = 2000, I0 = 10, tmax = 100, g = 0.5, n = 0.1)
#'   # You should then use the simulation result returned from the function, e.g. like this:
#'   plot(result[,1],result[,2],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the shiny app 'ReproductiveNumber', which is part of this package, contains more details on the model.
#' @references See e.g. Keeling and Rohani 2008 for SIR models and the
#'   documentation for the deSolve package for details on ODE solvers
#' @author Andreas Handel
#' @export


simulate_reproductivenumber <- function(S0 = 1000, I0 = 1, f = 0.0, e = 0.0, tmax = 300, g = 10, b = 1e-2, m = 0, n = 0, w = 0){

  S0eff = (1 - f*e) * S0;
  R0 = f*e * S0;
  Y0 = c(S = S0eff, I = I0, R = R0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(b = b, g = g, m = m, n = n, w = w);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = reproductivenumberode, parms=pars, method = "vode", atol=1e-8, rtol=1e-8);

  return (odeoutput)
}
