#' Simulation of a compartmental infectious disease transmission model illustrating 
#' parasite infection dynamics with intermediate hosts 
#'
#' @description  This model allows for the simulation of a parasitic infection that requires 
#' an intermediate host for transmission
#' 
#' @param Sh : initial number of susceptible definitive hosts : numeric
#' @param Ih : initial number of infected definitive hosts : numeric
#' @param E : initial number of pathogens in the environment : numeric
#' @param Si : initial number of susceptible intermediate hosts : numeric
#' @param Ii : initial number of infected intermediate hosts : numeric
#' @param tmax : maximum simulation time : numeric
#' @param bi : rate of transmission from infected intermediate host to susceptible host : numeric
#' @param be : rate of transmission from environment to susceptible intermediate host : numeric
#' @param m : the rate of births of intermediate hosts : numeric
#' @param n : the rate of natural intermediate hosts : numeric
#' @param g : the rate at which infected hosts recover/die : numeric
#' @param w : the rate at which host immunity wanes in host : numeric
#' @param p : rate at which infected host shed the pathogen in the environment : numeric
#' @param c : rate at which the pathogen decays in the environment : numeric
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
#'   result <- simulate_parasites_ode()
#'   # To choose parameter values other than the standard one, 
#'   # specify the parameters you want to change, e.g. like such:
#'   result <- simulate_parasites_ode(Sh = 2000, Ih = 10, tmax = 100, g = 0.5)
#'   # You should then use the simulation result returned from the function, like this:
#'   plot(result$ts[ , "time"],result$ts[ , "Sh"],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the app 'Parasite Model', which is part of the DSAIDE package, contains more details.
#' @author Andreas Handel, Christine Casey
#' @export

simulate_parasites_ode <- function(Sh = 1e3, Ih = 1, E = 1, Si = 0, Ii = 0, tmax = 120, bi = 0.01, be = 0.01, m = 0, n = 0, g = 0, w = 0, p = 0.01, c = 0.001)
{
  
  # This function is used in the solver function and has no independent usages
  parasiteodes <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
      {
        
        #the ordinary differential equations
        dSh = - Sh * bi * Ii + (w * Rh) #susceptible definitive hosts
        dIh = Sh * bi * Ii  - (g * Ih) #infected and symptomatic definitive hosts
        dRh = g * Ih - (w * Rh) #recovered and immune definitive hosts 
        dE  = p * Ih - (c *E) # pathogens in the environment  
        dSi = m - n * Si - (be * E * Si) #susceptible intermediate hosts
        dIi = Si*be * E - (n * Ii) #infected intermediate hosts
        
        list(c(dSh, dIh, dRh, dE, dSi, dIi)) 
      }
    ) #close with statement
  } #end function specifying the ODEs
  
  ############################################################
  Y0 = c(Sh = Sh, Ih = Ih, Rh = 0, E=E, Si = Si, Ii = Ii);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)
  
  
  ############################################################
  #vector of parameters which is sent to the ODE function  
  pars=c(bi = bi, be = be, m = m, n = n, g = g, w = w, p = p, c = c); 

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::lsoda(Y0, timevec, func = parasiteodes, parms=pars, atol=1e-12, rtol=1e-12);

  colnames(odeoutput) <- c('time',"Sh","Ih","Rh","E","Si","Ii")
  result <- list()
  result$ts <- as.data.frame(odeoutput)
  return(result)
}
