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
#' @param S : initial number of susceptible hosts : numeric
#' @param I : initial number of infected hosts : numeric
#' @param R : initial number of recovered hosts : numeric
#' @param b : level/rate of infectiousness for hosts in the I compartment : numeric
#' @param g : rate at which a person leaves the I compartment : numeric
#' @param f : strength of intervention effort between 0 and 1 : numeric
#' @param tstart : time at which intervention effort starts : numeric
#' @param tend : time at which intervention effort ends : numeric
#' @param tmax : maximum simulation time : numeric
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
#'   result <- simulate_idcontrolmultioutbreak_ode()
#'   # To choose parameter values other than the standard one, 
#'   # specify the parameters you want to change, e.g. like such:
#'   result <- simulate_idcontrolmultioutbreak_ode(S = 2000, I = 10, tmax = 100, g = 0.5)
#'   # You should then use the simulation result returned from the function, like this:
#'   plot(result$ts[ , "time"],result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the app 'Multi Outbreak ID Control', which is part of the DSAIDE package, contains more details.
#' @author Andreas Handel
#' @export


simulate_idcontrolmultioutbreak_ode <- function(S = 1000, I = 1, R = 0, b = 1e-2, g = 5, f = 0.3, tstart = 10, tend = 50, tmax = 100){

  
  ############################################################
  # start function that specifies differential equations used by deSolve
  idcontrolmultioutbreak_ode <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and parms by name
      {    
        f = ifelse( (t>=tstart &&  t<=tend) ,f,0) #set f to zero if outside treatment interval
        
        #the ordinary differential equations
        dS = - b*(1-f)*S*I #susceptibles
        dI = b*(1-f)*S*I - g*I #infected/infectious
        dR = g*I #recovered
        
        list(c(dS, dI, dR))
      }
    ) #close with statement
  } #end function specifying the ODEs
  ############################################################
  
  Y0 = c(S = S, I = I, R = R);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #combining parameters into a parameter vector
  pars = c(b = b, g = g, f = f, tstart = tstart, tend = tend);

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::ode(y = Y0, times = timevec, func = idcontrolmultioutbreak_ode, parms=pars, method = "vode", atol=1e-8, rtol=1e-8);
  
  result <- list()
  result$ts <- as.data.frame(odeoutput)

  return(result)
}
