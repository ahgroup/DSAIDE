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
#'   transmission coefficient, which is assumed to have a period of a year.
#'
#' @param S : initial number of susceptible hosts : numeric
#' @param P : initial number of infected, pre-symptomatic hosts : numeric
#' @param bP : level/rate of infectiousness for hosts in the P compartment : numeric
#' @param bA : level/rate of infectiousness for hosts in the A compartment : numeric
#' @param bI : level/rate of infectiousness for hosts in the I compartment : numeric
#' @param s : strength of seasonal/annual sigmoidal variation of transmission rate : numeric
#' @param gP : rate at which a person leaves the P compartment : numeric
#' @param gA : rate at which a person leaves the A compartment : numeric
#' @param gI : rate at which a person leaves the I compartment : numeric
#' @param f : fraction of pre-symptomatic individuals that have an asymptomatic infection : numeric
#' @param d : fraction of symptomatic infected hosts that die due to disease : numeric
#' @param w : rate at which recovered persons lose immunity and return to susceptible state : numeric
#' @param n : the rate at which new individuals enter the model (are born) : numeric
#' @param m : the rate of natural death (the inverse it the average lifespan) : numeric
#' @param timeunit : units of time in which the model should run (1=day, 2=week, 3=month, 4=year) : numeric
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
#'   the code will likely abort with an error message.
#' @examples
#'   # To run the simulation with default parameters just call the function:
#'   result <- simulate_idpatterns_ode()
#'   # To choose parameter values other than the standard one, specify them like such:
#'   result <- simulate_idpatterns_ode(S = 2000, P = 10, tmax = 100, f = 0.1, d = 0.2, s = 0.1)
#'   # You should then use the simulation result returned from the function, like this:
#'   plot(result$ts[ , "time"],result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the app, which is part of this package, contains more details on the model.
#' @references See e.g. Keeling and Rohani 2008 for SIR models and the
#'   documentation for the deSolve package for details on ODE solvers
#' @author Andreas Handel
#' @export

simulate_idpatterns_ode <- function(S = 1000, P = 1, bP = 0, bA = 0, bI = 0.002, s = 0, gP = 1, gA = 1, gI = 1, f = 0, d = 0, w = 0, n = 0, m = 0, timeunit = 1, tmax = 300)
{

  ############################################################
  # start function that specifies differential equations used by deSolve
  idpatternsode <- function(t, y, parms)
  {

    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
      {
        #seasonally varying transmission parameters
        bPs <- bP * (1 + s * sin(2*pi*t/tunit) )
        bAs <- bA * (1 + s * sin(2*pi*t/tunit) )
        bIs <- bI * (1 + s * sin(2*pi*t/tunit) )

        #the ordinary differential equations
        dS =  n - S * (bPs * P + bAs * A + bIs * I) + w * R - m *S; #susceptibles
        dP =    S * (bPs * P + bAs * A + bIs * I) - gP * P - m * P; #infected, pre-symptomatic
        dA =  f*gP*P - gA * A - m * A #infected, asymptomatic
        dI =  (1-f)*gP*P -  gI*I - m * I #infected, symptomatic
        dR =  (1-d)*gI*I + gA * A - w * R - m * R #recovered, immune
        dD = d*gI*I #dead

        list(c(dS, dP, dA, dI, dR, dD))
      }
    ) #close with statement
  } #end function specifying the ODEs
  ############################################################

  Y0 = c(S = S, P = P, A = 0, I = 0, R = 0, D = 0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  tunitvec=c(365,52,12,1) #depending on choice of units (days/weeks/months/years), pick divisor for annual variation in transmission in the ODEs
  tunit=tunitvec[timeunit]


  #combining parameters into a parameter vector
  pars = c(bP = bP, bA = bA, bI = bI, gP = gP , gA = gA, gI = gI, f = f, d = d, w = w, m = m, n = n, s = s , tunit = tunit);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = idpatternsode, parms=pars, method = "vode", atol=1e-12, rtol=1e-12);
  result <- list()
  result$ts <- as.data.frame(odeoutput)

  return(result)
}
