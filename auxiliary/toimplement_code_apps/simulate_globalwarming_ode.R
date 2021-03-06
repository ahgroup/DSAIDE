#' Simulation of a vector-borne infectious disease transmission model with changing temperature
#'
#' @description  This model allows for the simulation of a vector-borne infectious disease.
#' Transmission is temperature-dependent and the user can change the temperature dynamics.
#' 
#'
#' @param Sh0 : initial number of susceptible hosts 
#' @param Ih0 : initial number of infected hosts
#' @param Sv0 : initial number of susceptible vectors 
#' @param Iv0 : initial number of infected vectors
#' @param tmax : maximum simulation time, units of months
#' @param b1 : rate of transmission from infected vector to susceptible host
#' @param b2 : rate of transmission from infected host to susceptible vector
#' @param m0 : birth rate of vectors to susceptible host
#' @param m1 : rate of transmission from infected host to susceptible vector

#' @param g the rate at which infected hosts recover

n = 0.1, g = 1, w = 0, timeunit = 1, W0 = 15, W1 = 0 , W2 = 0 , W3 = 0, p=25, Vmax = 3000)
  

#' @param m the rate of births of vectors
#' @param n the rate of natural death of vectors
#' @param w the rate at which host immunity wanes
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The compartments are Sh, Ih, Rh, and Sv, Iv.
#'   The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#' @section Warning:
#'   This function does not perform any error checking. So if you try to do
#'   something nonsensical (e.g. any negative values or fractions > 1),
#'   the code will likely abort with an error message
#' @examples
#'   # To run the simulation with default parameters just call this function
#'   result <- simulate_vectortransmission()
#'   # To choose parameter values other than the standard one, specify them e.g. like such
#'   result <- simulate_vectortransmission(Sh0 = 100, Sv0 = 1e5,  tmax = 100)
#'   # You should then use the simulation result returned from the function, e.g. like this:
#'   plot(result$ts[ , "Time"],result$ts[ , "Sh"],xlab='Time',ylab='Number Susceptible',type='l')
#' @seealso The UI of the shiny app 'VectorTransmission', which is part of this package, contains more details on the model
#' @author Andreas Handel
#' @references See the information in the corresponding shiny app for model details
#'            See the documentation for the deSolve package for details on ODE solvers
#' @export



simulate_globalwarming_ode <- function(Sh = 1000, Ih = 0, Sv = 1000, Iv = 0, tmax = 1000, b1 = 0.00, b2 = 0.00, m0 = 0.1, m1 = 0, n = 0.1, g = 1, w = 0, timeunit = 1, W0 = 15, W1 = 0 , W2 = 0 , W3 = 0, p=25, Vmax = 3000)
{
  
  # This function is used in the solver function and has no independent usages
  vectortempeq <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
      {
        
        W =  W0 + (1 + W3*t/tunit)*W1*sin(2*pi*t/tunit) + W2*t/tunit        
        
        #the ordinary differential equations
        dSh =  - Sh * b1 * Iv + w * Rh; #susceptibles
        dIh =  Sh * b1 * Iv  - g * Ih #infected, symptomatic
        dRh =   g * Ih - w * Rh #recovered, immune
        
        dSv = (m1*Sv + m0*(Sv+Iv)) * ((p/W)*(1-((Sv+Iv)/Vmax))) - n*(W/p)*(1-((Sv+Iv)/Vmax)) * Sv - b2 * Ih * Sv; #susceptible vectors
        dIv = b2 * Ih * Sv + m1*(p/W)*Iv*(1-((Sv+Iv)/Vmax)) - n*(W/p) *(1-((Sv+Iv)/Vmax)) * Iv 
        
        
        list(c(dSh, dIh, dRh, dSv, dIv))
      }
    ) #close with statement
  } #end function specifying the ODEs
  
  
  
  ############################################################
  Y0 = c(Sh = Sh, Ih = Ih, Rh = 0, Sv = Sv, Iv = Iv);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)
  
  tunitvec=c(365,52,12,1) #depending on choice of units (days/weeks/months/years), pick divisor for annual variation in transmission in the ODEs
  tunit=tunitvec[timeunit]
  
  ############################################################
  #vector of parameters which is sent to the ODE function  
  pars=c(b1 = b1, b2 = b2, m0 = m0, m1 = m1, n = n, g = g, w = w, tunit = tunit, W0=W0, W1=W1, W2=W2, W3=W3, p=p, Vmax=Vmax); 

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::lsoda(Y0, timevec, func = vectortempeq, parms=pars, atol=1e-12, rtol=1e-12);

  W =  W0 + (1 + W3*timevec/tunit)*W1*sin(2*pi*timevec/tunit) + W2*timevec/tunit        
  
  odeoutput = cbind(odeoutput,W)
  
  colnames(odeoutput) <- c('time',"Sh","Ih","Rh","Sv","Iv","W")
  result <- list()
  result$ts <- as.data.frame(odeoutput)
  return(result)
}
