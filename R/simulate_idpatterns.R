# This function is used in the ode solver function and has no independent usages
idpatternsode <- function(t, y, parms)
{

  with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
    {
      #seasonally varying transmission parameters
      bPs <- bP * (1 + s * sin(2*pi*t/12) )
      bAs <- bA * (1 + s * sin(2*pi*t/12) )
      bIs <- bI * (1 + s * sin(2*pi*t/12) )

      #the ordinary differential equations
      dS =  m - S * (bPs * P + bAs * A + bIs * I) + w * R - n *S; #susceptibles
      dP =    S * (bPs * P + bAs * A + bIs * I) - gP * P - n * P; #infected, pre-symptomatic
      dA =  f*gP*P - gA * A - n * A #infected, asymptomatic
      dI =  (1-f)*gP*P -  gI*I - n * I #infected, symptomatic
      dR =  (1-d)*gI*I + gA * A - w * R - n * R #recovered, immune
      dD = d*gI*I #dead

      list(c(dS, dP, dA, dI, dR, dD))
    }
  ) #close with statement

} #end function specifying the ODEs

#' Simulation of a compartmental infectious disease transmission model including seasonality
#'
#' @description  Simulation of a compartmental model with several different compartments:
#'   Susceptibles (S), Infected and Pre-symptomatic (P),
#'   Infected and Asymptomatic (A), Infected and Symptomatic (I),
#'   Recovered and Immune (R) and Dead (D).
#'
#'   This model includes natural births and deaths and waning immunity.
#'   It also allows for seasonal variation in transmission.
#'   The model is assumed to run in units of months.
#'   This assumption is hard-coded into the sinusoidally varying
#'   transmission coefficient, which is assumed to have a period of a year
#'   
#'
#' @param PopSize specifies the initial number of individuals
#'   (Suceptibles + Infected & Pre-symptomatic)
#'   All other compartments start at 0
#' @param P0 initial number of infected, pre-symptomatic hosts,
#' @param bP level/rate of infectiousness for hosts in the P compartment
#' @param bA level/rate of infectiousness for hosts in the A compartment
#' @param bI level/rate of infectiousness for hosts in the I compartment
#' @param s strength of seasonal/annual sigmoidal variation of transmission rate
#' @param gP rate at which a person leaves the P compartment, which
#'   is the inverse of the average time spent in that compartment
#' @param gA rate at which a person leaves the A compartment
#' @param gI rate at which a person leaves the I compartment
#' @param f fraction of pre-symptomatic individuals that have an asymptomatic infection
#' @param d fraction of symptomatic infected hosts that die due to disease
#' @param w rate at which recovered persons loose immunity and return to susceptible state
#' @param m the rate at which new individuals enter the model (are born)
#' @param n the rate of natural death (the inverse it the average lifespan)
#' @param tmax maximum simulation time, in units of months
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
#'   result <- simulate_idpatterns()
#'   # To choose parameter values other than the standard one, specify them e.g. like such
#'   result <- simulate_idpatterns(PopSize = 2000, P0 = 10, tmax = 100, f = 0.1, d = 0.2, s = 0.1)
#'   # You should then use the simulation result returned from the function, e.g. like this:
#'   plot(result[,1],result[,2],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the shiny app 'IDPatterns', which is part of this package, contains more details on the model
#' @references See e.g. Keeling and Rohani 2008 for SIR models and the
#'   documentation for the deSolve package for details on ODE solvers
#' @author Andreas Handel
#' @export

simulate_idpatterns <- function(PopSize = 1000, P0 = 1, tmax = 300, bP = 0, bA = 0, bI = 1/1000, gP = 0.5, gA = 0.5, gI = 0.5, f = 0, d = 0, w = 0, m = 0, n = 0, s = 0)
{
  S0 = PopSize - P0; #initial number of uninfected hosts
  Y0 = c(S = S0, P = P0, A = 0, I = 0, R = 0, D = 0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(bP = bP, bA = bA, bI = bI, gP = gP , gA = gA, gI = gI, f = f, d = d, w = w, m = m, n = n, s = s);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = idpatternsode, parms=pars, method = "vode", atol=1e-12, rtol=1e-12);


  #The output produced by a call to the odesolver is odeoutput matrix is returned by the function
  return(odeoutput)
}
