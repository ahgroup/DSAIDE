#' Fitting a simple SIR type model to norovirus outbreak data
#'
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The model describes a simple SIR model with an additional
#' environmental source of infection
#' The user provides initial conditions and parameter values for the system.
#' The function simulates the ODE using an ODE solver from the deSolve package.
#'
#' @param S : starting value for Susceptible : numeric
#' @param I : starting value for Infected : numeric
#' @param R : starting value for Recovered : numeric
#' @param b : infection rate : numeric
#' @param blow : lower bound for infection rate : numeric
#' @param bhigh : upper bound for infection rate : numeric
#' @param g : recovery rate : numeric
#' @param glow : lower bound for g : numeric
#' @param ghigh : upper bound for g : numeric
#' @param n : rate of infection from common source : numeric
#' @param nlow : lower bound for n : numeric
#' @param nhigh : upper bound for n : numeric
#' @param t1 : start time of infection from common source : numeric
#' @param t2 : end time of infection from common source: numeric
#' @param fitmodel : fitting model variant 1, 2 or 3 : numeric
#' @param iter : max number of steps to be taken by optimizer : numeric
#' @param solvertype : the type of solver/optimizer to use (1-3) : numeric
#' @return The function returns a list containing the best fit timeseries,
#' the best fit parameters, the data and the AICc for the model.
#' @details Three versions of a simple SIR type compartmental ODE model
#' are fit to cases of norovirus during an outbreak.
#' #' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter or starting values),
#'   the code will likely abort with an error message.
#' @examples
#' # To run the code with default parameters just call the function:
#' \dontrun{result <- simulate_noro_fit()}
#' # To apply different settings, provide them to the simulator function, like such:
#' result <- simulate_noro_fit(iter = 5, fitmodel = 2)
#' @seealso See the Shiny app documentation corresponding to this
#' function for more details on this model.
#' @author Andreas Handel
#' @importFrom nloptr nloptr

#' @export

simulate_noro_fit <- function(S = 100, I = 1, R = 0, b = 1e-3, blow = 1e-10, bhigh = 1e-1,  g = 0.5, glow = 1e-3, ghigh = 1e2,  n = 0, nlow = 0, nhigh = 1e3, t1 = 8, t2 = 15, fitmodel = 1, iter = 100, solvertype = 1)

{

  ###################################################################
  #function specifying the ode model.
  #This is called by the ode solver inside the fit function
  ###################################################################

  noro_model_ode <- function(t, y, parms, fitmodel)
    {
      with( as.list(c(y,parms)), { #lets us access variables and parameters stored in y and parms by name

        #we are fitting 3 different models, depending on the scenario variable
        #by default (fitmodel 2), n is 'on' and constant
        if (fitmodel==1)
        {
          n=0; #no external source
        }
        if (fitmodel==3)
        {
          if (t1 > t || t > t2) {n=0} #with step-function environmental source
        }

        dS = - n*S  - b*I*S
        dI = n*S + b*I*S - g*I
        dR =  g*I
        list(c(dS,dI,dR))
      } )
    } #close with statement, end ODE code block


  ###################################################################
  #function that fits the ODE model to data
  ###################################################################
  modelcompfitfunction <- function(params, fitdata, Y0, xvals, fitmodel, fitparnames)
  {

    names(params) = fitparnames #for some reason nloptr strips names from parameters
    #call ode-solver lsoda to integrate ODEs

    odeout <- try(deSolve::ode(y = Y0, times = xvals, func = noro_model_ode, parms=params, atol=1e-8, rtol=1e-8, fitmodel = fitmodel));

    #extract values at time points where data is available
    modelpred = odeout[match(fitdata$xvals,odeout[,"time"]),"I"];

    #return the objective function, the sum of squares,
    #which is being minimized by the optimizer
    return(sum((modelpred-fitdata$outcome)^2))

  } #end function that fits the ODE model to the data


  ############################################################
  #the main function, which calls the fit function
  ############################################################

  #some settings for ode solver and optimizer
  #those are hardcoded here, could in principle be rewritten to allow user to pass it into function
  atolv=1e-8; rtolv=1e-8; #accuracy settings for the ODE solver routine

  #load data
  fitdata =  norodata
  colnames(fitdata) = c("xvals",'outcome')

  Y0 = c(S = S, I = I, R = R)  #combine initial conditions into a vector
  xvals = seq(min(fitdata$xvals), max(fitdata$xvals), 0.1); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  if (fitmodel == 1)
  {
    par_ini = as.numeric(c(b=b, g=g))
    lb = as.numeric(c(blow, glow))
    ub = as.numeric(c(bhigh, ghigh))
    fitparnames = c('b','g')
  }

  if (fitmodel == 2)
  {
    par_ini = as.numeric(c(b=b, g=g, n=n))
    lb = as.numeric(c(blow, glow, nlow))
    ub = as.numeric(c(bhigh, ghigh, nhigh))
    fitparnames = c('b','g','n')
  }
  if (fitmodel == 3)
  {
    par_ini = as.numeric(c(b=b, g=g, n=n, t1=t1, t2=t2))
    lb = as.numeric(c(blow, glow, nlow,min(xvals),min(xvals))) #last 2 are low/high for start/end times
    ub = as.numeric(c(bhigh, ghigh, nhigh,max(xvals),max(xvals)))
    fitparnames = c('b','g','n','t1','t2')
  }

  if (solvertype == 1) {algname = "NLOPT_LN_COBYLA"}
  if (solvertype == 2) {algname = "NLOPT_LN_NELDERMEAD"}
  if (solvertype == 3) {algname = "NLOPT_LN_SBPLX"}

  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  bestfit = nloptr::nloptr(x0=par_ini, eval_f=modelcompfitfunction,lb=lb,ub=ub,opts=list("algorithm"=algname,xtol_rel=1e-10,maxeval=iter,print_level=0), fitdata=fitdata, Y0 = Y0, xvals = xvals, fitmodel=fitmodel, fitparnames=fitparnames)


  #extract best fit parameter values and from the result returned by the optimizer
  params = bestfit$solution
  names(params) = fitparnames #for some reason nloptr strips names from parameters

  #time-series for best fit model
  odeout <- try(deSolve::ode(y = Y0, times = xvals, func = noro_model_ode, parms=params, atol=1e-8, rtol=1e-8, fitmodel = fitmodel));

  #compute sum of square residuals (SSR) for initial guess and final solution
  modelpred = odeout[match(fitdata$xvals,odeout[,"time"]),"I"];

  ssrfinal=(sum((modelpred-fitdata$outcome)^2))

  #compute AICc
  N=length(fitdata$outcome) #number of datapoints
  K=length(par_ini); #fitted parameters for model
  AICc= N * log(ssrfinal/N) + 2*(K+1)+(2*(K+1)*(K+2))/(N-K)

  #adjust data a bit for consistent formatting on return
  fitdata$varnames = "I_data"
  colnames(fitdata) = c("xvals",'yvals','varnames')

  #list structure that contains all output
  result = list()
  result$ts = odeout
  result$bestpars = params
  result$AICc = AICc
  result$SSR = ssrfinal
  result$data = fitdata

  #The output produced by the fitting routine
  return(result)
}
