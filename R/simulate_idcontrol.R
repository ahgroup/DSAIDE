# This function is used in the ode solver function and has no independent usages
idcontrolode <- function(t, y, parms)
{

  with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
    {
 
      #the ordinary differential equations
      dS =  birth_h - S * (bP * P + bA * A + bI * I + bE * E + bv * Iv) + w * R - death_h *S; #susceptibles
      dP =    S * (bP * P + bA * A + bI * I + bE * E + bv * Iv) - gP * P - death_h * P; #infected, pre-symptomatic
      dA =  f*gP*P - gA * A - death_h * A #infected, asymptomatic
      dI =  (1-f)*gP*P -  gI*I - death_h * I #infected, symptomatic
      dR =  (1-d)*gI*I + gA * A - w * R - death_h * R #recovered, immune
      dD = d*gI*I #dead

      dE = pI * I + pA * A - c * E; #pathogen in environment
      
      dSv = birth_v - death_v * Sv - bh * I * Sv; #susceptible vectors
      dIv = bh * I * Sv - death_v * Iv ; #susceptible hosts
      
      
      list(c(dS, dP, dA, dI, dR, dD, dE, dSv, dIv))
    }
  ) #close with statement

} #end function specifying the ODEs

#' Simulation of a compartmental infectious disease transmission model including different control mechanisms
#'
#' @description  Simulation of a compartmental model with several different compartments:
#'   Susceptibles (S), Infected and Pre-symptomatic (P),
#'   Infected and Asymptomatic (A), Infected and Symptomatic (I),
#'   Recovered and Immune (R) and Dead (D).
#'   Also modeled is an environmental pathogen stage (E), and susceptible (Sv) and infected (Iv) vectors.
#'
#'   Any initial conditions not specified below start at 0.
#'
#' @param S0 initial number of susceptible hosts
#' @param I0 initial number of infected and symptomatic hosts
#' @param E0 initial amount of pathogen in environment
#' @param Sv0 initial number of susceptible vectors
#' @param Iv0 initial number of infected vectors
#' @param bP rate of transmission from pre-symptomatic to susceptible hosts
#' @param bA rate of transmission from asymptomatic to susceptible hosts
#' @param bI rate of transmission from symptomatic to susceptible hosts
#' @param bE rate of transmission from environment to susceptible hosts
#' @param bv rate of transmission from infected vectors to susceptible hosts
#' @param bh rate of transmission from symptomatic hosts to susceptible vectors
#' @param gP rate at which a person leaves the P compartment, which
#'   is the inverse of the average time spent in that compartment
#' @param gA rate at which a person leaves the A compartment
#' @param gI rate at which a person leaves the I compartment
#' @param pA rate of pathogen shedding into environment by asymptomatic hosts
#' @param pI rate of pathogen shedding into environment by symptomatic hosts
#' @param c rate of pathogen decay in environment 
#' @param f fraction of pre-symptomatic individuals that have an asymptomatic infection
#' @param d fraction of symptomatic infected hosts that die due to disease
#' @param w rate at which recovered persons loose immunity and return to susceptible state
#' @param birth_h the rate at which new hosts enter the model (are born)
#' @param death_h the rate of natural death of hosts (the inverse it the average lifespan)
#' @param birth_v the rate at which new vectors enter the model (are born)
#' @param death_v the rate of natural death of vectors (the inverse it the average lifespan)
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
#'   result <- simulate_idcontrol()
#'   # To choose parameter values other than the standard one, specify them e.g. like such
#'   result <- simulate_idcontrol(S0 = 2000, I0 = 10, tmax = 100, f = 0.1, d = 0.2)
#'   # You should then use the simulation result returned from the function, e.g. like this:
#'   plot(result[,1],result[,2],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the shiny app 'IDPatterns', which is part of this package, contains more details on the model
#' @references See e.g. Keeling and Rohani 2008 for SIR models and the
#'   documentation for the deSolve package for details on ODE solvers
#' @author Andreas Handel
#' @export

simulate_idcontrol <- function(S0 = 1000, I0 = 1, E0 = 0, Sv0 = 1000, Iv0 = 0, tmax = 300, bP = 0, bA = 0, bI = 1/1000, bE = 0, bv = 1/1000, bh = 1/1000, gP = 0.5, gA = 0.5, gI = 0.5, pA = 1, pI = 10, c = 1,  f = 0, d = 0, w = 0, birth_h = 0, death_h = 0, birth_v = 0, death_v = 0)
{
  Y0 = c(S = S0, P = 0, A = 0, I = I0, R = 0, D = 0, E = E0, Sv = Sv0, Iv = Iv0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(bP = bP, bA = bA, bI = bI, bE = bE, bv = bv, bh = bh, gP = gP , gA = gA, gI = gI, pA = pA, pI = pI, c = c, f = f, d = d, w = w, birth_h = birth_h, death_h = death_h, birth_v = birth_v, death_v = death_v);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = idcontrolode, parms=pars, method = "vode", atol=1e-12, rtol=1e-12);

  #The output produced by a call to the odesolver is odeoutput matrix is returned by the function
  return(odeoutput)
}
