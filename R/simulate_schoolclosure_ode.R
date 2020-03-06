#' Simulation of a compartmental infectious disease transmission model with 3 types of hosts and intervention
#'
#' @description  This model allows for the simulation of an ID with 3 types of hosts.
#' Groups are assumed to be children, adults and seniors.
#' Intervention can be applied to any of the groups for a certain duration.
#'
#'
#' @param Sc : initial number of susceptible children : numeric
#' @param Ic :  initial number of infected children : numeric
#' @param Sa :  initial number of susceptible adults : numeric
#' @param Ia :  initial number of infected adults : numeric
#' @param Ss :  initial number of susceptible seniors : numeric
#' @param Is :  initial number of infected seniors : numeric
#' @param bcc :  rate of transmission from infected child to susceptible child : numeric
#' @param bca :  rate of transmission from infected child to susceptible adult : numeric
#' @param bcs :  rate of transmission from infected child to susceptible senior : numeric
#' @param bac :  rate of transmission from infected adult to susceptible child : numeric
#' @param baa :  rate of transmission from infected adult to susceptible adult : numeric
#' @param bas :  rate of transmission from infected adult to susceptible senior : numeric
#' @param bsc :  rate of transmission from infected senior to susceptible child : numeric
#' @param bsa :  rate of transmission from infected senior to susceptible adult : numeric
#' @param bss :  rate of transmission from infected senior to susceptible senior : numeric
#' @param gc :  rate at which infected children recover or die : numeric
#' @param ga :  rate at which infected adults recover or die : numeric
#' @param gs :  rate at which infected seniors recover or die : numeric
#' @param dc :  fraction of infected children who die : numeric
#' @param da :  fraction of infected adults who die : numeric
#' @param ds :  fraction of infected seniors who die : numeric
#' @param c_start : start of intervention : numeric
#' @param c_end : end of intervention : numeric
#' @param c_strength : strength of intervention, between 0 and 1 : numeric
#' @param c_group : target group for intervention (1 = children, 2 = adults, 3 = seniors) : numeric
#' @param tnew : time at which new infected adults enter : numeric
#' @param tmax :  maximum simulation time, units of months : numeric
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver.
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#'   The model implement basic processes of infection, recovery and death.
#'   Treatment is applied, which reduces b by the indicated proportion, during times tstart and tend.
#'   At time intervals given by tnew, a new infected individual enters the population.
#'   The simulation also monitors the number of infected and when they drop below 1, they are set to 0.
#' @section Warning:
#'   This function does not perform any error checking. So if you try to do
#'   something nonsensical (e.g. any negative values or fractions > 1),
#'   the code will likely abort with an error message.
#' @examples
#'   # To run the simulation with default parameters just call the function:
#'   result <- simulate_schoolclosure_ode()
#' @author Andreas Handel
#' @export


simulate_schoolclosure_ode <- function(Sc = 1e3, Ic = 1, Sa = 1e3, Ia = 1, Ss = 1e3, Is = 0, bcc = 1e-3, bca = 1e-3, bcs = 1e-3, bac = 1e-3, baa = 1e-3, bas = 1e-3, bsc = 1e-3, bsa = 1e-3, bss = 1e-3, gc = 0.1, ga = 0.1, gs = 0.1, dc = 0.001, da = 0.01, ds = 0.1, c_start = 30, c_end = 60, c_strength = 0.5, c_group = 1, tnew  = 30, tmax = 365)
{

  # This function is used in the solver function and has no independent usages
  schoolclosureeq <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
      {

        #apply intervention
        if (t>=c_start &&  t<=c_end)
        {
          if (c_group==1) { bcc = (1 - c_strength) * bcc; bca = (1 - c_strength) * bca; bcs = (1 - c_strength) * bcs;  }
          if (c_group==2) { bac = (1 - c_strength) * bac; baa = (1 - c_strength) * baa; bas = (1 - c_strength) * bas;  }
          if (c_group==3) { bsc = (1 - c_strength) * bsc; bsa = (1 - c_strength) * bsa; bss = (1 - c_strength) * bss;  }
        }


        #the ordinary differential equations
        dSc =  - Sc * (bcc * Ic + bac * Ia + bsc * Is)
        dIc =    Sc * (bcc * Ic + bac * Ia + bsc * Is) - gc * Ic
        dRc =   (1-dc)*gc * Ic
        dDc =   dc*gc*Ic

        dSa =  - Sa * (bca * Ic + baa * Ia + bsa * Is)
        dIa =    Sa * (bca * Ic + baa * Ia + bsa * Is) - ga * Ia
        dRa =   (1-da)*ga * Ia
        dDa =   da*ga*Ia

        dSs =  - Ss * (bcs * Ic + bas * Ia + bss * Is)
        dIs =    Ss * (bcs * Ic + bas * Ia + bss * Is) - gs * Is
        dRs =   (1-ds)*gs * Is
        dDs =   ds*gs*Is


        list(c(dSc, dIc, dRc, dDc, dSa, dIa, dRa, dDa, dSs, dIs, dRs, dDs))
      }
    ) #close with statement
  } #end function specifying the ODEs

  ############################################################
  # first function monitors each infected compartment, if numbers drop below 1, second function is triggered which sets to 0
  # prevents fractional number of infected
  checkinfected <- function(t,y,parms)
  {
    c(y["Ic"]-0.99, y["Ia"]-0.99, y["Is"]-0.99) #check if any infected compartment drops below 1
  }
  zeroinfected <- function(t,y,parms)
  {
    y["Ic"] = ifelse( y["Ic"] < 1, 0 , y["Ic"]) #set compartment to 0 if it is less than 1
    y["Ia"] = ifelse( y["Ia"] < 1, 0 , y["Ia"]) #set compartment to 0 if it is less than 1
    y["Is"] = ifelse( y["Is"] < 1, 0 , y["Is"]) #set compartment to 0 if it is less than 1
    return(y)
  }



  ############################################################
  Y0 = c(Sc = Sc, Ic = Ic, Rc = 0, Dc = 0, Sa = Sa, Ia = Ia, Ra = 0, Da = 0, Ss = Ss, Is = Is, Rs = 0, Ds = 0);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  #browser()

  ############################################################
  #vector of parameters which is sent to the ODE function
  pars=c(bcc = bcc, bca = bca, bcs = bcs, bac = bac, baa = baa, bas = bas, bsc = bsc, bsa = bsa , bss = bss, gc = gc, ga = ga, gs = gs, dc = dc, da = da, ds = ds, c_start = c_start, c_end = c_end, c_strength = c_strength, c_group = c_group)


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
    odetmp = deSolve::ode(y = Y0, times = timevec, func = schoolclosureeq, parms=pars, method = "lsoda", events = list(func = zeroinfected, root = TRUE), rootfun = checkinfected, atol=1e-8, rtol=1e-8);
    odeoutput = rbind(odeoutput,odetmp) #not very efficient but ok way to save all results
    ts = tf #new starting time is last ending time
    Y0 = odetmp[nrow(odetmp),-1] #values of variables at last step of simulation
    Y0["Ia"] = Y0["Ia"] + 1 #add one infected adult
  }
  #one more simulation bit to the end
  if (ts<tmax)
  {
    timevec = seq(ts,tmax,dt)
    odetmp = deSolve::ode(y = Y0, times = timevec, func = schoolclosureeq, parms=pars, method = "lsoda", events = list(func = zeroinfected, root = TRUE), rootfun = checkinfected, atol=1e-8, rtol=1e-8);
    odeoutput = rbind(odeoutput,odetmp) #not very efficient but ok way to save all results
  }

  result <- list()
  result$ts <- as.data.frame(odeoutput)
  return(result)
}
