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
#' @param S : initial number of susceptible hosts : numeric
#' @param I : initial number of infected and symptomatic hosts : numeric
#' @param E : initial amount of pathogen in environment : numeric
#' @param Sv : initial number of susceptible vectors : numeric
#' @param Iv : initial number of infected vectors : numeric
#' @param bP : rate of transmission to susceptible hosts from pre-symptomatic : numeric
#' @param bA : rate of transmission to susceptible hosts from asymptomatic : numeric
#' @param bI : rate of transmission to susceptible hosts from symptomatic : numeric
#' @param bE : rate of transmission to susceptible hosts from environment : numeric
#' @param bv : rate of transmission to susceptible hosts from infected vectors : numeric
#' @param bh : rate of transmission to susceptible vectors from symptomatic hosts : numeric
#' @param gP : rate at which a person leaves the P compartment : numeric
#' @param gA : rate at which a person leaves the A compartment : numeric
#' @param gI : rate at which a person leaves the I compartment : numeric
#' @param pA : rate of pathogen shedding into environment by asymptomatic hosts : numeric
#' @param pI : rate of pathogen shedding into environment by symptomatic hosts : numeric
#' @param c : rate of pathogen decay in environment : numeric
#' @param f : fraction of pre-symptomatic individuals that will go on to be asymptomatic : numeric
#' @param d : fraction of symptomatic infected hosts that die due to disease : numeric
#' @param w : rate at which recovered persons lose immunity and return to susceptible state : numeric
#' @param nh : the rate at which new hosts enter the model (are born) : numeric
#' @param mh : the rate of mortality of hosts (the inverse it the average lifespan) : numeric
#' @param nv : the rate at which new vectors enter the model (are born) : numeric
#' @param mv : the rate of mortality of vectors (the inverse it the average lifespan) : numeric
#' @param tmax : maximum simulation time : numeric
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver.
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#' @section Warning:
#'   This function does not perform any error checking. So if you try to do
#'   something nonsensical (e.g. have I0 > PopSize or any negative values or fractions > 1),
#'   the code will likely abort with an error message
#' @examples
#'   # To run the simulation with default parameters just call the function:
#'   result <- simulate_idcontrol_ode()
#'   # To choose parameter values other than the standard one, specify them like such:
#'   result <- simulate_idcontrol_ode(S = 2000, I = 10, tmax = 100, f = 0.1, d = 0.2)
#'   # You should then use the simulation result returned from the function, like this:
#'   plot(result$ts[ , "time"], result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the Shiny app 'IDPatterns', which is part of this package, contains more details on the model.
#' @references See e.g. Keeling and Rohani 2008 for SIR models and the
#'   documentation for the deSolve package for details on ODE solvers
#' @author Andreas Handel
#' @export

simulate_idcontrol_ode <- function(S = 1000, I = 1, E = 0, Sv = 1000, Iv = 0, bP = 0, bA = 0, bI = 2e-3, bE = 0, bv = 0, bh = 0, gP = 2, gA = 1, gI = 1, pA = 0, pI = 0, c = 0,  f = 0, d = 0, w = 0, nh = 0, mh = 0, nv = 0, mv = 0, tmax = 60)
{

  # This function is used in the ode solver function and has no independent usages
  idcontrolode <- function(t, y, parms)
  {

    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
      {

        #the ordinary differential equations
        dS =  nh - S * (bP * P + bA * A + bI * I + bE * E + bv * Iv) + w * R - mh *S; #susceptibles
        dP =    S * (bP * P + bA * A + bI * I + bE * E + bv * Iv) - gP * P - mh * P; #infected, pre-symptomatic
        dA =  f*gP*P - gA * A - mh * A #infected, asymptomatic
        dI =  (1-f)*gP*P -  gI*I - mh * I #infected, symptomatic
        dR =  (1-d)*gI*I + gA * A - w * R - mh * R #recovered, immune
        dD = d*gI*I #dead
        dE = pI * I + pA * A - c * E; #pathogen in environment
        dSv = nv - mv * Sv - bh * I * Sv; #susceptible vectors
        dIv = bh * I * Sv - mv * Iv ; #susceptible hosts

        list(c(dS, dP, dA, dI, dR, dD, dE, dSv, dIv))
      }
    ) #close with statement

  } #end function specifying the ODEs

    Y0 = c(S = S, P = 0, A = 0, I = I, R = 0, D = 0, E = E, Sv = Sv, Iv = Iv);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(bP = bP, bA = bA, bI = bI, bE = bE, bv = bv, bh = bh, gP = gP , gA = gA, gI = gI, pA = pA, pI = pI, c = c, f = f, d = d, w = w, nh = nh, mh = mh, nv = nv, mv = mv);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the remaining columns are the variables
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = idcontrolode, parms=pars, method = "vode", atol=1e-12, rtol=1e-12);

  result <- list()
  result$ts <- as.data.frame(odeoutput)

  return(result)
}
