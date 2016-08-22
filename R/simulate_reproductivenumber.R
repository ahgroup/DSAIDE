
# reproductive_number_ode_eq function
# This function is used in the solver function and has no independent usages
reproductivenumberode <- function(t, y, pars)
{
      S = y[1]; In = y[2]; Rec = y[3]; #assigning the y/variable vector to each of the compartments - susceptibles (S), infecteds (I), recovereds (R)
      beta = pars[1]; gamma = pars[2]; #assigning the parameter vector to each of the parameter values - level of infectiousness (beta), duration of infectious period (1/gamma)
      lambda = pars[3]; n = pars[4]; #birth and death rates
      w = pars[5]; #immunity loss rate

      #the ordinary differential equations
  	  dS = lambda - n * S - beta * S * In +  w * Rec; #susceptibles
	  	dIn = beta * S * In - gamma * In - n * In; #infected/infectious
	 	  dRec = gamma * In - n * Rec - w * Rec; #recovered

      return(list(c(dS, dIn, dRec)))
} #end function specifying the ODEs


#' Simulation of a compartmental infectious disease transmission model to study the reproductive number
#' 
#' @description  Simulation of a basic SIR compartmental model with these compartments:
#'   Susceptibles (S), Infected/Infectious (I),
#'   Recovered and Immune (R).
#'   
#'   The model is assumed to be in units of months when run through the Shiny App
#'   However as long as all parameters are chosen in the same units, 
#'   one can directly call the simulator assuming any time unit
#'
#' @param PopSize specifies the initial number of individuals
#'   (Suceptibles + Infected)#'
#' @param I0 initial number of infected hosts
#' @param f fraction of vaccinated individuals. Those individuals are moved from S to R at the beginning of the simulation
#' @param e efficay of vaccine, given as fraction between 0 and 1
#' @param beta level/rate of infectiousness for hosts in the I compartment
#' @param gamma rate at which a person leaves the I compartment
#' @param w rate at which recovered persons loose immunity and return to susceptible state
#' @param lambda the rate at which new individuals enter the model (are born)
#' @param n the rate of natural death (the inverse it the average lifespan)
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#' @section Warning:
#'   This function does not perform any error checking. So if you try to do
#'   something nonsensical (e.g. have I0 > PopSize or any negative values or fractions > 1),
#'   the code will likely abort with an error message
#' @examples
#'   # To run the simulation with default parameters just call this function
#'   result <- simulate_reproductivenumber()
#'   # To choose parameter values other than the standard one, specify them e.g. like such
#'   result <- simulate_reproductivenumber(PopSize = 2000, I0 = 10, tmax = 100, gamma = 0.5, n = 0.1)
#'   # You should then use the simulation result returned from the function, e.g. like this:
#'   plot(result[,1],result[,2],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the shiny app 'ReproductiveNumber', which is part of this package, contains more details on the model
#' @references See e.g. Keeling and Rohani 2008 for SIR models and the
#'   documentation for the deSolve package for details on ODE solvers
#' @author Andreas Handel
#' @export


simulate_reproductivenumber <- function(PopSize = 1000, I0 = 0, f = 0.0, e = 0.0, tmax = 300, gamma = 50, beta = 1e-1, lambda = 20, n = 1/50, w = 0){

  S0 = PopSize - I0; #initial number of uninfected hosts
  S0eff = (1 - f*e) * S0;
  R0 = f*e * S0;
  Y0 = c(S = S0eff, I = I0, R = R0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(beta, gamma, lambda, n, w);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = reproductivenumberode, parms=pars, method = "vode", atol=1e-8, rtol=1e-8);

  return (odeoutput)
}
