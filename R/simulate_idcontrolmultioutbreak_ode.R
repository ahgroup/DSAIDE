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
#' @param b : rate of new infections : numeric
#' @param g : rate of recovery : numeric
#' @param f : strength of intervention effort : numeric
#' @param tstart : time at which intervention effort starts : numeric
#' @param tend : time at which intervention effort ends : numeric
#' @param tnew : time at which new infected enter : numeric
#' @param tmax : maximum simulation time : numeric
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver.
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#'   The model implement basic processes of infection at rate b and recovery at rate g.
#'   Treatment is applied, which reduces b by the indicated proportion, during times tstart and tend.
#'   At time intervals given by tnew, a new infected individual enters the population.
#'   The simulation also monitors the number of infected and when they drop below 1, they are set to 0.
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

simulate_idcontrolmultioutbreak_ode <- function(S = 1000, I = 1, R = 0, b = 1e-3, g = 1, f = 0.3, tstart = 10, tend = 50, tnew = 50, tmax = 100){

  
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

  
  ############################################################
  # functions that monitor I and sets it to 0 if it crosses the 1 threshold 
  checkinfected <- function(t,y,parms)
  {
    y["I"]-0.99
  }
  zeroinfected <- function(t,y,parms)
  {
    y["I"] = 0
    return(y)
  }
      
  Y0 = c(S = S, I = I, R = R);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back

  #combining parameters into a parameter vector
  pars = c(b = b, g = g, f = f, tstart = tstart, tend = tend, tnew = tnew);

  #if times of immigration is set larger than max simulation time, set them to simulation time max
  if (tnew>tmax) {tnew=tmax}
  newinftimes = seq(tnew,tmax,by=tnew) #times at which a new infected enters the population
  
  #since the desolve/ode do not handle multiple events/roots (as far as I know)
  #we need to do the entry of new infected at times tnew 'by hand' and integrate piecewise
  #easiest to do with a loop
  odeoutput = NULL
  ts = 0; #start pieces of integration at 0  
  for (n in 1:length(newinftimes))
  {
    tf = newinftimes[n] #simulate to time of first new infected entering
    timevec = seq(ts,tf,dt)
    odetmp = deSolve::ode(y = Y0, times = timevec, func = idcontrolmultioutbreak_ode, parms=pars, method = "lsoda", events = list(func = zeroinfected, root = TRUE), rootfun = checkinfected, atol=1e-8, rtol=1e-8);
    odeoutput = rbind(odeoutput,odetmp) #not very efficient but ok for here way to save all results
    ts = tf #new starting time is last ending time
    Y0 = odetmp[nrow(odetmp),-1] #values of variables at last step of simulation
    Y0["I"] = Y0["I"] + 1 #add one infected
  }
  #one more simulation bit to the end
  if (ts<tmax)
  {
    timevec = seq(ts,tmax,dt)
    odetmp = deSolve::ode(y = Y0, times = timevec, func = idcontrolmultioutbreak_ode, parms=pars, method = "lsoda", events = list(func = zeroinfected, root = TRUE), rootfun = checkinfected, atol=1e-8, rtol=1e-8);
    odeoutput = rbind(odeoutput,odetmp) #not very efficient but ok for here way to save all results
  }
  #browser()
  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  
  result <- list()
  result$ts <- as.data.frame(odeoutput)

  return(result)
}
