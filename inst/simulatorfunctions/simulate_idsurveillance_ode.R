#' Simulation of a compartmental infectious disease transmission model illustrating 
#' the impact of ID surveillance  
#'
#' @description  This model allows for the exploration of the impact of ID surveillance on transmission dynamics
#' 
#' @param S : initial number of susceptible hosts : numeric
#' @param P : initial number of infected pre-symptomatic hosts : numeric
#' @param tmax : maximum simulation time : numeric
#' @param bP : rate of transmission from presymptomatic to susceptible host : numeric
#' @param bA : rate of transmission from asymptomatic to susceptible host : numeric
#' @param bI : rate of transmission from symptomatic to susceptible host : numeric
#' @param gP : the rate at which presymptomatic hosts move to the next stage : numeric
#' @param f : fraction of asymptomatic hosts : numeric
#' @param d : rate at which infected hosts die : numeric
#' @param w : the rate at which host immunity wanes : numeric
#' @param m : the rate of births : numeric
#' @param n : the rate of natural deaths : numeric
#' @param rP : rate of pre-symptomatic host removal due to surveillance : numeric
#' @param rA : rate of asymptomatic host removal due to surveillance : numeric
#' @param rI : rate of symptomatic host removal due to surveillance : numeric
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
#'   # To run the simulation with default parameters just call the function:
#'   result <- simulate_idsurveillance_ode()
#'   # To choose parameter values other than the standard one, 
#'   # specify the parameters you want to change, e.g. like such:
#'   result <- simulate_idsurveillance_ode(S = 2000, tmax = 100, f = 0.5)
#'   # You should then use the simulation result returned from the function, like this:
#'   plot(result$ts[ , "time"],result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the app 'Parasite Model', which is part of the DSAIDE package, contains more details.
#' @author Andreas Handel, Ronald Galiwango
#' @export


simulate_idsurveillance_ode <- function(S = 1000, P = 1, tmax = 200, bP = 0, bA = 0, bI = 0.001, gP = 0.5, f = 0, d = 0, w = 0, m = 0, n = 0, rP = 0, rA = 0, rI = 0.5)
{

  # reproductive_number_ode_eq function
  # This function is used in the solver function and has no independent usages
  idsurveillanceode <- function(t, y, parms)
  {
    
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
      {
        #the ordinary differential equations
        dS =  m - S * (bP * P + bA * A + bI * I) + w*R - n*S  #susceptibles
        dP =    S * (bP * P + bA * A + bI * I) - P*(gP + rP + n) #infected, pre-symptomatic
        dA =  f*gP*P - A*(rA + n)  #infected, asymptomatic
        dI =  (1-f)*gP*P -  I*(rI + n + d) #infected, symptomatic
        dR =  rI*I + rA*A + rP*P - w*R #Observed, removed
        dD =  d*I #Mortality, Dead
        
        list(c(dS, dP, dA, dI, dR, dD))
      }
    ) #close with statement
    
  } #end function specifying the ODEs
  
  
  Y0 = c(S = S, P = P, A = 0, I = 0, R = 0, D = 0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(bP = bP, bA = bA, bI = bI, gP = gP , rP = rP, rA = rA, rI = rI, f = f, d = d, w = w, m = m, n = n);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = idsurveillanceode, parms=pars, atol=1e-12, rtol=1e-12);

  result <- list()
  result$ts <- as.data.frame(odeoutput)
  
  #The output produced by a call to the odesolver is odeoutput matrix is returned by the function
  return(result)
}
