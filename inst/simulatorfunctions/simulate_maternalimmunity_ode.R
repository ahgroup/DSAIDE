#' Simulation of a MSEIR model that represents a group of the population that is protected by disease through
#' maternal antibodies
#' 
#' @description This function runs a simulation of a MSEIR model
#' using a set of 6 ordinary differential equations.
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#' The function returns a matrix containing time-series of each variable and time.
#'
#' @param S : initial number of susceptible individuals : numeric
#' @param I : initial number of infected hosts : numeric
#' @param tmax : maximum simulation time : numeric
#' @param m : the rate at which individuals are born : numeric
#' @param n : the rate at which individuals die : numeric
#' @param p : the rate at which individuals lose passive immunity
#' @param b : rate of new infections : numeric
#' @param gE : rate of leaving latent stage : numeric
#' @param gI : rate of recovery : numeric
#' @param w : rate of waning immunity : numeric
#' @return The function returns the output from the odesolver as a matrix,
#' with one column per compartment/variable. The first column is time.
#' @details A simple MSEIR model is simulated as a set of ordinary differential
#' equations, using an ode solver from the deSolve package.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- simulate_maternalimmunity_ode()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_maternalimmunity_ode(S = 2000, I = 10, tmax = 100, b = 0.2)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result$ts[ , "time"], result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the deSolve
#' package for details on the underlying ODE simulator algorithm.
#' @author Chase Golden, Andreas Handel
#' @export

simulate_maternalimmunity_ode <- function(S = 1000, I = 1, tmax = 1000, m = 0.02, n = 0.02, p = 0.005, b = 0.3, gE = 0.1, gI = 0.02, w = 0)
{

  # This function is used in the solver function and has no independent usages
  maternalode <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
      {
        
        
        
        #the ordinary differential equations
        dM = m * (N - S) - (p + n) * M #passively immune
        dS = m * S + p * M - b * S * I / N - n * S + w * R #susceptibles
        dE = b * S * I / N - (gE + n) * E #exposed, but not infectious
        dI = gE * E - (gI + n) * I #infectious
        dR = gI * I - (w + n) * R #recovered
        dN = (m - n) * N
        
        list(c(dM, dS, dE, dI, dR, dN))
      }
    ) #close with statement
  } #end function specifying the ODEs
  
  
  Y0 = c(M = 0, S = S, E = 0, I = I, R = 0, N = S + I);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(m = m, n = n, p = p, b = b, gE = gE, gI = gI, w = w);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = maternalode, parms=pars, atol=1e-12, rtol=1e-12);
  
  #return result as list, with element ts containing the time-series
  result <- list()
  result$ts <- as.data.frame(odeoutput)

  return(result)
}
