#' Simulation of an infectious disease transmission model with multiple compartments
#'
#' @description  Simulation of a compartmental model with several different compartments:
#' Susceptibles (S), Infected and Pre-symptomatic (P),
#' Infected and Asymptomatic (A), Infected and Symptomatic (I),
#' Recovered and Immune (R) and Dead (D)
#'
#' @param S : initial number of susceptible hosts : numeric
#' @param P : initial number of infected, pre-symptomatic hosts : numeric
#' @param bP : level/rate of infectiousness for hosts in the P compartment : numeric
#' @param bA : level/rate of infectiousness for hosts in the A compartment : numeric
#' @param bI : level/rate of infectiousness for hosts in the I compartment : numeric
#' @param gP : rate at which a person leaves the P compartment, which : numeric
#' @param gA : rate at which a person leaves the A compartment : numeric
#' @param gI : rate at which a person leaves the A compartment : numeric
#' @param f : fraction of pre-symptomatic individuals that have an asymptomatic infection : numeric
#' @param d : fraction of symptomatic infected hosts that die due to disease : numeric
#' @param tmax : maximum simulation time : numeric
#' @return The function returns the output from the odesolver as a matrix,
#' with one column per compartment/variable. The first column is time.
#' @details A compartmental ID model with several states/compartments
#' is simulated as a set of ordinary differential equations. The states
#' are:
#' **S**: Susceptible, uninfected individuals
#' **P**: Presymptomatic individuals who are infected and possibly infectious
#' **A**: Asymptomatic individuals who are infected and possibly infectious
#' **I**: Sympomatic infected individuals, most likely infectious
#' **R**: Removed / recovered individuals, no longer infectious or susceptible
#' **D**: Individuals who have died from the disease
#' The model app contains detailed information on the processes, but briefly,
#' susceptible (S) individuals can become infected by presymptomatic (P), asymptomatic (A),
#' or infected (I) hosts. All infected individuals enter the presymptomatic stage first,
#' from which they can become symptomatic or asymptomatic. Asymptomatic hosts recover
#' within some specified duration of time, while infected hosts either recover or die,
#' thus entering either R or D. Recovered individuals are immune to reinfection.
#' @section Warning:
#' This function does not perform any error checking. So if you try to do
#' something nonsensical (e.g. have negative values for parameters),
#' the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' result <- simulate_idcharacteristics_ode()
#' # To choose parameter values other than the standard one, specify them like such:
#' result <- simulate_idcharacteristics_ode(S = 2000, P = 10, tmax = 100, f = 0.1, d = 0.2)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$ts[,"time"],result$ts[,"S"],xlab='Time',ylab='Number Susceptible',type='l')
#' @references See e.g. Keeling and Rohani 2008 for SIR models and the
#'   documentation for the deSolve package for details on ODE solvers
#' @author Andreas Handel
#' @export

simulate_idcharacteristics_ode <- function(S = 1000, P = 1, bP = 0, bA = 0, bI = 0.001, gP = 0.5, gA = 0.5, gI = 0.5, f = 0, d = 0, tmax = 300)
{
  
  ############################################################
  # start function that specifies differential equations used by deSolve
  idcharacteristicsode <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
      {
        #the ordinary differential equations
        dS =  - S * (bP * P + bA * A + bI * I)  #susceptibles
        dP =    S * (bP * P + bA * A + bI * I) - gP * P #infected, pre-symptomatic
        dA =  f*gP*P - gA * A #infected, asymptomatic
        dI =  (1-f)*gP*P -  gI*I #infected, symptomatic
        dR =  (1-d)*gI*I + gA * A  #recovered, immune
        dD = d*gI*I #dead
        
        list(c(dS, dP, dA, dI, dR, dD))
      }
    ) #close with statement
  } #end function specifying the ODEs
  ############################################################
  
  
  
  
  Y0 = c(S = S, P = P, A = 0, I = 0, R = 0, D = 0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(bP = bP, bA = bA, bI = bI, gP = gP , gA = gA, gI = gI, f = f, d = d);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = idcharacteristicsode, parms=pars, atol=1e-12, rtol=1e-12);
  result <- list()
  result$ts <- as.data.frame(odeoutput)
  
  #The output produced by a call to the odesolver is odeoutput matrix is returned by the function
  return(result)
}
