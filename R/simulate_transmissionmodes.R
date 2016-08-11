
# This function is used in the solver function and has no independent usages
transmissionmode <- function(t, y, pars)
{
      #Programming Note - instead of passing parameters to the function via the pars argument and then picking them up inside the function,
      #i'm just using the fact that anything defined in the main program is also accessible to the functions. This is not very good - but convenient - programming
      #just make sure you keep track of value definitions. Also note that the reverse doesn't work: What you define inside a function stays in there
      #unless you explicitly send it back out to the calling program

      S = y[1]; I = y[2]; R = y[3]; #assigning the y/variable vector to each of the compartments - susceptibles (S), infecteds (I), recovereds (R)
      N = S + I + R; #total population size

      #force of infection for different scenarios: 1 = density-dependent, 2 = frequency-dependent
      A = 1; #set value for area - pretty much arbitrary for the ODE model since there is no real space present anyway
      #A=k*N; #area proportional to population size - could do A=k*sqrt(N), A=k&N^2, etc.
      if (pars[1] == 1) {lambda = pars[2] * I / A;}
      if (pars[1] == 2) {lambda = pars[3] * I / N;}
      if (pars[1] == 3) {lambda = pars[3] * I / sqrt(N);} #intermediate between density- and frequency- dependence. Now more infected (larger population) leads to a higher force of infection, but not as extreme as for I/A formulation.

      #the ordinary differential equations - includes birth-death and waning immunity
  	  dS = pars[4] - pars[5] * S - lambda * S + pars[6] * R; #susceptibles
      dI = lambda * S - pars[7] * I - pars[5] * I; #infected/infectious
	    dR = pars[7] * I - pars[5] * R - pars[6] * R; #recovered

      return(list(c(dS, dI, dR)));
} #end function specifying the ODEs

#' Simulation of a compartmental infectious disease transmission model illustrating different types of transmission
#'
#' @description  This model allows for the simulation of different transmission modes
#' 
#'
#' @param PopSize try a range of values for this and see how it affects the outbreak - this corresponds to the "initial-population" slider in NetLogo
#' @param I0 initial number of infected hosts, again try a number of different values - this corresponds to the "initial-infected" slider in NetLogo
#' @param R0 initial number of recovered hosts - for now we assume there are no recovered (immune) hosts. We get to this in the course.
#' @param tmax maximum simulation time, units of years, (equivalent to Max-time slider in the NetLogo simulation)
#' @param gamma rate at which a person leaves the infectious compartment, which is the inverse of the average duration of the infections period. Note: 1/gamma is the duration of the infectious period in units of YEARS so e.g. gamma=26 means infectious period = 1/26 years = 14 days
#' @param beta.d rate of transmission of pathogen from infected to susceptible host (density)
#' @param beta.f rate of transmission of pathogen from infected to susceptible host (frequency)
#' @param mu death rate parameter - inverse, i.e. 1/mu, is the average lifespan (in years), a value of 0 means no deaths. A value of 1/20 means e.g. 20 years lifespan
#' @param w duration of immunity. Setting it to zero means infinite immunity (no waning immunity), 1/w is duration of immunity, so e.g. w=0.5 means 2 years of immunity
#' @param k constant of proportionality for population-size dependent area - choose such that results look decent
#' @param scenario set to different values for different stransmission scenarios: 1 = density-dependent, 2 = frequency-dependent
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
#'   result <- simulate_transmissionmodes()
#'   # To choose parameter values other than the standard one, specify them e.g. like such
#'   result <- simulate_transmissionmodes(PopSize = 2000, P0 = 10, tmax = 100 )
#'   # You should then use the simulation result returned from the function, e.g. like this:
#'   plot(result[,1],result[,2],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the shiny app 'TransmissionModes', which is part of this package, contains more details on the model
#' @author Andreas Handel
#' @references We can add (a) reference(s) here..
#' @export


simulate_directtransmission <- function(PopSize = 1e6, I0 = 1, R0 = 0, tmax = 5, gamma = 13, beta.d = 4e-5, beta.f = 40,  mu = 0 * 1 / 50,
                              w = 0.0, k = 1e-6, scenario = 2){
  ############################################################
  #setting initial conditions for variables
  #you can change initial conditions by changing the values assigned to PopSize and I0
  S0 = PopSize - I0; #initial number of uninfected hosts
  Y0 = c(S0, I0, R0);  #combine initial conditions into a vector

  ############################################################
  #specifying maximum simulation/integration time and time vector for which we want solutions
  #you can change the maximum time for which the simulation is run by changing the value assigned to tmax
  timevec = seq(0, tmax, 0.05); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  ############################################################
  #specify values for the model parameters.
  #transmission and infection duration parameters - you can change the parameters by changing the values assigned to each

  #birth rate parameter - number of births per year
  birth=0*2*mu*PopSize; #a value of 0 means no births

  parameters=c(scenario, beta.d, beta.f, birth, mu, w, gamma); #vector of parameters which is sent to the ODE function
  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::lsoda(Y0, timevec, transmission_type_ode_eq, parms=parameters, atol=1e-12, rtol=1e-12);

  return (odeoutput)
}
