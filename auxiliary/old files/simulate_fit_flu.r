#' Fitting fitting mortality data from the 1918 influenza pandemic 
#' to an SIR model to estimate R0
#' 
#' @description This function runs a simulation of a compartment model
#' using a set of ordinary differential equations.
#' The model describes a simple viral infection system.
#' @param U : initial number of uninfected target cells : numeric
#' @param I : initial number of infected target cells : numeric
#' @param V : initial number of infectious virions : numeric
#' @param X : initial level of immune response : numeric
#' @param n : rate of uninfected cell production : numeric
#' @param dU : rate at which uninfected cells die : numeric
#' @param dI : rate at which infected cells die : numeric
#' @param g : unit conversion factor : numeric
#' @param p : rate at which infected cells produce virus : numeric
#' @param plow : lower bound for p : numeric
#' @param phigh : upper bound for p : numeric
#' @param psim : rate at which infected cells produce virus for simulated data : numeric
#' @param b : rate at which virus infects cells : numeric
#' @param blow : lower bound for infection rate : numeric
#' @param bhigh : upper bound for infection rate : numeric
#' @param bsim : rate at which virus infects cells for simulated data : numeric
#' @param dV : rate at which infectious virus is cleared : numeric
#' @param dVlow : lower bound for virus clearance rate : numeric
#' @param dVhigh : upper bound for virus clearance rate : numeric
#' @param dVsim : rate at which infectious virus is cleared for simulated data : numeric
#' @param noise : noise to be added to simulated data : numeric
#' @param iter : max number of steps to be taken by optimizer : numeric
#' @param solvertype : the type of solver/optimizer to use (1-3) : numeric
#' @param usesimdata : set to 1 if simulated data should be fitted, 0 otherwise : numeric
#' @return The function returns a list containing as elements the best fit time series data frame, the best fit parameters,
#' the data and the final SSR
#' @details A simple compartmental ODE model mimicking acute viral infection
#' is fitted to data.
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
#' \dontrun{result <- simulate_basicmodel_fit()}
#' # To apply different settings, provide them to the simulator function, like such:
#' result <- simulate_basicmodel_fit(iter = 5)
#' @seealso See the Shiny app documentation corresponding to this
#' function for more details on this model.
#' @author Andreas Handel
#' @importFrom utils read.csv
#' @importFrom dplyr filter rename select
#' @importFrom nloptr nloptr
#' @export


simulate_basicmodel_fit <- function(U = 1e5, I = 0, V = 1, X = 1, n = 0, dU = 0, dI = 1, g = 1, p = 10, plow = 1e-3, phigh = 1e3,  psim = 10, b = 1e-4, blow = 1e-6, bhigh = 1e-3,  bsim = 1e-4, dV = 5, dVlow = 1e-3, dVhigh = 1e3,  dVsim = 5, noise = 0, iter = 100, solvertype = 1, usesimdata = 1)
{
  

  #Block of ODE equations for deSolve 
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
  fitfunction <- function(params, fitdata, Y0, xvals, fixedpars, fitparnames)
  {
    
    names(params) = fitparnames #for some reason nloptr strips names from parameters
    Y0[2]=params[3]; #initial number of infected, is being fitted 
    
    allpars = c(Y0, params, tfinal = max(xvals), 
    timevec=seq()
                                dt = 0.1, tstart = 0, fixedpars)
    
    #call ode-solver lsoda to integrate ODEs 
    odeoutput=try(lsoda(Y0,timevec,odeequations,appars,atol=1e-10));
    deathmodel=odeoutput[seq(11,151,10),4]; #extract values for dead at time points corresponding to data values, i.e. every week  
    
    #return the objective function, the sum of squares, which is being minimized  
    SSR=sum((deathmodel-fitdata$outcome)^2) #linear scale
    if (logfit==1) #log scale
    {
      SSR=sum((log10(deathmodel)-log10(fitdata$outcome))^2) #fit is done on a log scale
    }
    return(SSR) 
  } #end function that fits the ODE model to the data
  
      
  
  
  #will contain final result
  output <- list()
  
  #some settings for ode solver and optimizer
  #those are hardcoded here, could in principle be rewritten to allow user to pass it into function
  atolv=1e-8; rtolv=1e-8; #accuracy settings for the ODE solver routine
  maxsteps = iter #number of steps/iterations for algorithm
  
  #load data
  #experimental data values from Mills et al. 2004 Nature 
  filename = system.file("extdata", "flu1918data.csv", package = "DSAIDE")
  alldata = utils::read.csv(filename)
  #data is weekly new cases of death
  #We fit cumulative cases 
  fitdata =  c(alldata[,'Week'], cumsum(alldata[,'WeeklyDeaths'])) 
  colnames(fitdata) = c("xvals",'outcome')
  
  Y0 = c(U = U, I = I, V = V);  #combine initial conditions into a vector
  xvals = seq(0, max(fitdata$xvals), 0.1); #vector of times for which solution is returned (not that internal timestep of the integrator is different)
  
  #if we want to fit simulated data
  if (usesimdata == 1)
  {
    
    #combining fixed parameters and to be estimated parameters into a vector
    modelpars = c(n=n,dU=dU,dI=dI,dV=dVsim,b = bsim,p=psim,g=g);
    
    allpars = c(Y0,tfinal = max(fitdata$xvals), tstart = 0, dt = 0.1, modelpars)
    #simulate model with known parameters to get artifitial data
    #not sure why R needs it in such a weird form
    #but supplying vector of values to function directly doesn't work
    odeout <- do.call(DSAIRM::simulate_basicvirus_ode, as.list(allpars))
    simres = odeout$ts
    
    #extract values for virus load at time points where data is available
    simdata = data.frame(simres[match(fitdata$xvals,simres[,"time"]),])
    simdata$simres = log10(simdata$V)
    simdata = subset(simdata, select=c('time', 'simres'))
    colnames(simdata) = c('xvals','outcome')
    fitdata$outcome = simdata$outcome + noise*stats::runif(length(simdata$outcome),-1,1)*simdata$outcome
  }
  
  
  #combining fixed parameters and to be estimated parameters into a vector
  fixedpars = c(n=n,dU=dU,dI=dI,g=g)
  
  par_ini = as.numeric(c(p, b, dV))
  lb = as.numeric(c(plow, blow, dVlow))
  ub = as.numeric(c(phigh, bhigh, dVhigh))
  fitparnames = c('p', 'b', 'dV')
  
  if (solvertype == 1) {algname = "NLOPT_LN_COBYLA"}
  if (solvertype == 2) {algname = "NLOPT_LN_NELDERMEAD"}
  if (solvertype == 3) {algname = "NLOPT_LN_SBPLX"}
  
  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, all other column the model variables
  #in the order they are passed into Y0 (which needs to agree with the order in virusode)
  bestfit = nloptr::nloptr(x0=par_ini, eval_f=basicfitfunction,lb=lb,ub=ub,opts=list("algorithm"=algname,xtol_rel=1e-10,maxeval=maxsteps,print_level=0), fitdata=fitdata, Y0 = Y0, xvals = xvals, fixedpars=fixedpars,fitparnames=fitparnames)
  
  
  #extract best fit parameter values and from the result returned by the optimizer
  params = bestfit$solution
  names(params) = fitparnames #for some reason nloptr strips names from parameters
  modelpars = c(params,fixedpars)
  
  allpars = c(Y0,modelpars,tfinal = max(fitdata$xvals))
  
  
  
  #doe one final run of the ODE to get a time-series to report back
  odeout <- do.call(simulate_basicvirus_ode, as.list(allpars))
  simres = odeout$ts
  #extract values for virus load at time points where data is available
  modelpred = simres[match(fitdata$xvals,simres[,"time"]),"V"];
  
  logvirus=c(log10(pmax(1e-10,modelpred)));
  ssrfinal=(sum((logvirus-fitdata$outcome)^2))
  
  #list structure that contains all output
  output$ts = odeout$ts
  output$bestpars = params
  output$SSR = ssrfinal
  
  output$data = fitdata
  
  #The output produced by the fitting routine
  return(output)
}



                




############################################################
#the main part, which calls the fit function 
############################################################

maxsteps=1000; #maximum number of iterations for the optimization routine 

#--> Find a value for Sus0, i.e. the number of susceptibles living in NYC in 1918
Sus0=5.6e6; #1918 NYC population - we assume everyone is susceptible
Dead0=0; #initial number of deaths
timevec=seq(0, 15,0.1); #vector of times (in units of weeks) for which integration is evaluated
#--> The data are deaths per 100,000. This needs to be converted to total deaths. Write a line that does this conversion, i.e. convert the data in cumdeaths into total deaths
deathdata=cumdeaths/100000*Sus0; #rescale death data for full population

#starting guesses for the parameters that will be fit. units are in weeks
b0=1e-6;  
Infc00=10; 
f0=0.2;   

gam=1; #we fix duration of infection, 1/gam to one week
ploton=1;  #switches on/off plotting during fitting 
#uses the optimization routine optim to perform the fit
x0=c(b=b0,f=f0,Infc0=Infc00);
fitresult <- optim(par=x0, fn=fitfunction, gr = NULL, timedata=timedata, deathdata=deathdata, method = "Nelder-Mead",control=list(trace=0,maxit=maxsteps,parscale=x0))
finalparams=fitresult$par

############################################################
#output result
############################################################

#compute model solution for initial parameter values
Y0=c(Sus0, Infc00, Dead0);  #combine initial conditions into a vector 
odeoutputini=lsoda(Y0,timevec,odeequations,x0);
deathmodelini=odeoutputini[seq(11,151,10),4]; 

#compute model solution for final parameter values
b=finalparams[1]; f=finalparams[2]; Infc0=finalparams[3]; 
Y0=c(Sus0, Infc0, Dead0);  #combine initial conditions into a vector 
odeoutput=lsoda(Y0,timevec,odeequations,c(b,f,Infc0));
deathmodel=odeoutput[seq(11,151,10),4]; 

ssrini=sum((log10(deathmodelini)-log10(deathdata))^2); ssrfinal=sum((log10(deathmodel)-log10(deathdata))^2);
#--> when you fit on the linear scale, also report final SSR in linear space
#ssrini=sum((deathmodelini-deathdata)^2); ssrfinal=sum((deathmodel-deathdata)^2);

#--> compute R0 
R0=b/gam*Sus0;

print(sprintf('Initial guess - Mortality fraction: %f, Initial Infected: %f, R0: %f, SSR=%f',f0,Infc00,b0/gam*Sus0,ssrini));
#--> print best fit values for f, Initial number of infected, and R0. Also print final SSR
print(sprintf('Best fit - Mortality fraction: %f, Initial Infected: %f, R0: %f, SSR=%f',f,Infc0,R0,ssrfinal));

#plot data and model solution for initial and final parameter values
graphics.off(); #close graphics window     
plot(timedata,log10(deathdata),type="p",xlim=c(0,15),ylim=c(-1,7),col="red",ylab="Deaths (log scale)",xlab="time (weeks)");
lines(odeoutputini[,1],log10(pmax(1e-10,odeoutputini[,4])),col="blue");
lines(odeoutput[,1],log10(pmax(1e-10,odeoutput[,4])),col="black")
legend('bottomright',c('data','model initial','model final'),col=c('red','blue','black'),pch=c(19,-1,-1),lty=c(-1,1,1),lwd=2)       
 
###################################################################
#end main program
###################################################################