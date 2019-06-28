#' Fitting a SIR-type model to flu data
#' 
#' @description Fitting fitting mortality data from the 1918 influenza pandemic 
#' to an SIR-type model to estimate R0. For the data, see 'flu1918data'.
#' 
#' @param S : starting value for Susceptible : numeric
#' @param I : starting value for Infected : numeric
#' @param D : starting value for Dead : numeric
#' @param b : infection rate : numeric
#' @param blow : lower bound for infection rate : numeric
#' @param bhigh : upper bound for infection rate : numeric
#' @param bsim : infection rate for simulated data : numeric
#' @param g : recovery rate : numeric
#' @param glow : lower bound for g : numeric
#' @param ghigh : upper bound for g : numeric
#' @param gsim : recovery rate for simulated data : numeric
#' @param f : fraction dying : numeric
#' @param flow : lower bound for f : numeric
#' @param fhigh : upper bound for f : numeric
#' @param fsim : fraction dying for simulated data : numeric
#' @param noise : noise to be added to simulated data : numeric
#' @param iter : max number of steps to be taken by optimizer : numeric
#' @param solvertype : the type of solver/optimizer to use (1-3) : numeric
#' @param usesimdata : set to 1 if simulated data should be fitted, 0 otherwise : numeric
#' @param logfit : set to 1 if the log of the data should be fitted, 0 otherwise : numeric
#' @return The function returns a list containing as elements the best fit time series data frame, the best fit parameters,
#' the data and the final SSR
#' @details A simple compartmental ODE model is fitted to data.
#' The model includes susceptible, infected, and dead compartments. 
#' The two processes that are modeled are infection and recovery. A fraction of recovered can die.
#' Data can either be real or created by running the model with known parameters and using the simulated data to
#' determine if the model parameters can be identified.
#' The fitting is done using solvers/optimizers from the nloptr package (which is a wrapper for the nlopt library).
#' The package provides access to a large number of solvers.
#' Here, we only implement 3 solvers, namely 1 = NLOPT_LN_COBYLA, 2 = NLOPT_LN_NELDERMEAD, 3 = NLOPT_LN_SBPLX
#' For details on what those optimizers are and how they work, see the nlopt/nloptr documentation.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values,
#'   the code will likely abort with an error message.
#' @examples
#' # To run the code with default parameters just call the function:
#' \dontrun{result <- simulate_fit_flu()}
#' # To apply different settings, provide them to the simulator function, like such:
#' result <- simulate_fit_flu(iter = 5, logfit = 1, solvertype = 2, usesimdata = 1)
#' @seealso See the Shiny app documentation corresponding to this
#' function for more details on this model.
#' @author Andreas Handel
#' @importFrom nloptr nloptr
#' @export


simulate_fit_flu <- function(S = 5e6, I = 1, D = 0, b = 1e-6, blow = 1e-10, bhigh = 1e-1,  bsim = 1e-4,  g = 1, glow = 1e-3, ghigh = 1e2,  gsim = 1, f = 1e-2, flow = 1e-5, fhigh = 0.5,  fsim = 0.01, noise = 0, iter = 100, solvertype = 1, usesimdata = 0, logfit = 0)
{
  
  ###################################################################
  #function that specifies the ODE model  
  ###################################################################
  flufit_model_ode <- function(t, y, parms) 
  {
    with( as.list(c(y,parms)), { #lets us access variables and parameters stored in y and parms by name 
      dS =  -b*I*S
      dI = b*I*S - g*I
      dD =  f*g*I
      list(c(dS,dI,dD)) 
    } ) } #close with statement, end ODE code block 
  
  ###################################################################
  #function that fits the ODE model to data 
  ###################################################################
  fitfunction <- function(params, fitdata, Y0, fitparnames, logfit)
  {
    
    names(params) = fitparnames #for some reason nloptr strips names from parameters
    timevec=seq(0,max(fitdata$xvals),by = 0.1)
    
    #call ode-solver lsoda to integrate ODEs 
    odeout=try(deSolve::lsoda(Y0,timevec,flufit_model_ode,params,atol=1e-10,rtol=1e-10))

    #extract values for dead at time points corresponding to data values, i.e. every week  
    modelpred = odeout[match(fitdata$xvals,odeout[,"time"]),"D"];
    
    #return the objective function, the sum of squares, which is being minimized  
    SSR=sum((modelpred-fitdata$outcome)^2) #linear scale
    if (logfit==1) #log scale
    {
      SSR=sum((log10(modelpred)-log10(fitdata$outcome))^2) #fit is done on a log scale
    }
    
    #browser()
    
    return(SSR) 
  } #end function that fits the ODE model to the data
  

  ###################################################################
  #main function 
  ###################################################################
  
  #will contain final result
  output <- list()
  
  #load data
  #experimental data values from Mills et al. 2004 Nature 
  #data is weekly new cases of death
  #We fit cumulative cases 
  #The data are deaths per 100,000. This needs to be converted to total deaths by rescaling with the population size, S. 
  deathdata=cumsum(flu1918data$Deaths/100000*S) 
  #data is reported as dates, for simplicity in fitting, 
  #we just label each week as 1-15
  timedata = seq(1,length(deathdata))
  fitdata =  data.frame(xvals = timedata, outcome = deathdata) 
  
  Y0 = c(S = S, I = I, D = D);  #combine initial conditions into a vector
  timevec = seq(0, max(fitdata$xvals), 0.1); #vector of times for which solution is returned (not the internal timestep of the integrator is different)
  
  #if we want to fit simulated data
  if (usesimdata == 1)
  {
    #combining fixed parameters and to be estimated parameters into a vector
    modelpars = c(b = bsim, g=gsim, f=fsim)
    #simulate model with known parameters to get artifitial data
    simres = try(deSolve::lsoda(Y0,timevec,flufit_model_ode,modelpars,atol=1e-10))
    
    #extract values for virus load at time points where data is available
    simdata = data.frame(simres[match(fitdata$xvals,simres[,"time"]),])
    simdata = simdata[,c('time', 'D')]
    colnames(simdata) = c('xvals','outcome')
    fitdata$outcome = simdata$outcome + noise*stats::runif(length(simdata$outcome),-1,1)*simdata$outcome
  }
  
  
  
  par_ini = as.numeric(c(b, g, f))
  lb = as.numeric(c(blow, glow, flow))
  ub = as.numeric(c(bhigh, ghigh, fhigh))
  fitparnames = c('b', 'g', 'f')
  
  if (solvertype == 1) {algname = "NLOPT_LN_COBYLA"}
  if (solvertype == 2) {algname = "NLOPT_LN_NELDERMEAD"}
  if (solvertype == 3) {algname = "NLOPT_LN_SBPLX"}
 
  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  bestfit = nloptr::nloptr(x0=par_ini, eval_f=fitfunction,lb=lb,ub=ub,opts=list("algorithm"=algname,xtol_rel=1e-10,maxeval=iter,print_level=0), fitdata=fitdata, Y0 = Y0, fitparnames=fitparnames, logfit = logfit)
  
  
  #extract best fit parameter values and from the result returned by the optimizer
  params = bestfit$solution
  names(params) = fitparnames #for some reason nloptr strips names from parameters
  
  #doe one final run of the ODE to get a time-series to report back
  simres = try(deSolve::lsoda(Y0,timevec,flufit_model_ode,params,atol=1e-10))

  #extract values for dead at time points corresponding to data values, i.e. every week  
  modelpred = simres[match(fitdata$xvals,simres[,"time"]),"D"];

  #return the objective function, the sum of squares, which is being minimized  
  ssrfinal=sum((modelpred-fitdata$outcome)^2) #linear scale
  if (logfit==1) #log scale
  {
    ssrfinal=sum((log10(modelpred)-log10(fitdata$outcome))^2) #fit is done on a log scale
  }
  
  #list structure that contains all output
  output$ts = simres
  output$bestpars = params
  output$SSR = ssrfinal
  output$data = fitdata
  
  #The output produced by the fitting routine
  return(output)
}



