# This function is used in the solver function and has no independent usages
vectortransmissioneq <- function(t, y, parms)
{
  with(
    as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
    {
      
      #the ordinary differential equations
      dSh =  - Sh * b1 * Iv + w * Rh; #susceptibles
      dIh =  Sh * b1 * Iv  - g * Ih #infected, symptomatic
      dRh =   g * Ih - w * Rh #recovered, immune
   
      dSv = m - n * Sv - b2 * Ih * Sv; #susceptible vectors
      dIv = b2 * Ih * Sv - n * Iv ; #susceptible hosts
            
      list(c(dSh, dIh, dRh, dSv, dIv))
    }
  ) #close with statement
} #end function specifying the ODEs

  
  
#' Simulation of a compartmental infectious disease transmission model illustrating vector-borne transmission
#'
#' @description  This model allows for the simulation of a vector-borne infectious disease
#' 
#'
#' @param Sh0 initial number of susceptible hosts 
#' @param Ih0 initial number of infected hosts
#' @param Sv0 initial number of susceptible vectors 
#' @param Iv0 initial number of infected vectors
#' @param tmax maximum simulation time, units of months
#' @param b1 rate of transmission from infected vector to susceptible host
#' @param b2 rate of transmission from infected host to susceptible vector
#' @param m the rate of births of vectors
#' @param n the rate of natural death of vectors
#' @param g the rate at which infected hosts recover/die
#' @param w the rate at which host immunity wanes
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver.
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The compartments are Sh, Ih, Rh, and Sv, Iv.
#'   The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#' @section Warning:
#'   This function does not perform any error checking. So if you try to do
#'   something nonsensical (e.g. any negative values or fractions > 1),
#'   the code will likely abort with an error message.
#' @examples
#'   # To run the simulation with default parameters just call the function:
#'   result <- simulate_vectortransmission()
#'   # To choose parameter values other than the standard one, specify them like such:
#'   result <- simulate_vectortransmission(Sh0 = 100, Sv0 = 1e5,  tmax = 100)
#'   # You should then use the simulation result returned from the function, like this:
#'   plot(result$ts[ , "Time"],result$ts[ , "Sh"],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the Shiny app 'VectorTransmission', which is part of this package, contains more details on the model.
#' @author Andreas Handel
#' @references See the information in the corresponding Shiny app for model details.
#'            See the documentation for the deSolve package for details on ODE solvers.
#' @export


simulate_vectortransmission <- function(Sh0 = 1e3, Ih0 = 1, Sv0 = 0, Iv0 = 0, tmax = 120, b1 = 0.01, b2 = 0, m = 0, n = 0, g = 1, w = 0)
{
  ############################################################
  Y0 = c(Sh = Sh0, Ih = Ih0, Rh = 0, Sv = Sv0, Iv = Iv0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)
  
  
  ############################################################
  #vector of parameters which is sent to the ODE function  
  pars=c(b1 = b1, b2 = b2, m = m, n = n, g = g, w = w); 

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::lsoda(Y0, timevec, func = vectortransmissioneq, parms=pars, atol=1e-12, rtol=1e-12);

  colnames(odeoutput) <- c('Time',"Sh","Ih","Rh","Sv","Iv")
  result <- list()
  result$ts <- as.data.frame(odeoutput)
  return(result)
}
