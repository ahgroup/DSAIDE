#' Simulation of a compartmental infectious disease transmission model with 3 types of hosts and intervention
#'
#' @description  This model allows for the simulation of an ID with 3 types of hosts.
#' Groups are assumed to be children, adults and elderly.
#' Intervention can be applied to any of the groups for a certain duration.
#'
#'
#' @param Sc : initial number of susceptible children : numeric
#' @param Ic :  initial number of infected children : numeric
#' @param Sa :  initial number of susceptible adults : numeric
#' @param Ia :  initial number of infected adults : numeric
#' @param Se :  initial number of susceptible elderly : numeric
#' @param Ie :  initial number of infected elderly : numeric
#' @param bcc :  rate of transmission to susceptible child from infected child : numeric
#' @param bca :  rate of transmission to susceptible child from infected adult : numeric
#' @param bce :  rate of transmission to susceptible child from infected elderly : numeric
#' @param bac :  rate of transmission to susceptible adult from infected child : numeric
#' @param baa :  rate of transmission to susceptible adult from infected adult : numeric
#' @param bae :  rate of transmission to susceptible adult from infected elderly : numeric
#' @param bec :  rate of transmission to susceptible elderly from infected child : numeric
#' @param bea :  rate of transmission to susceptible elderly from infected adult : numeric
#' @param bee :  rate of transmission to susceptible elderly from infected elderly : numeric
#' @param gc :  rate at which infected children recover or die : numeric
#' @param ga :  rate at which infected adults recover or die : numeric
#' @param ge :  rate at which infected elderly recover or die : numeric
#' @param wc :  rate at which immunity in children wanes : numeric
#' @param wa :  rate at which immunity in adults wanes : numeric
#' @param we :  rate at which immunity in elderly wanes : numeric
#' @param mc :  fraction of infected children who die : numeric
#' @param ma :  fraction of infected adults who die : numeric
#' @param me :  fraction of infected elderly who die : numeric
#' @param f1 : strength of intervention applied to children, between 0 and 1 : numeric
#' @param T1_start : start of intervention applied to children : numeric
#' @param T1_end : end of intervention applied to children : numeric
#' @param f2 : strength of intervention applied to adults, between 0 and 1 : numeric
#' @param T2_start : start of intervention applied to adults : numeric
#' @param T2_end : end of intervention applied to adults : numeric
#' @param f3 : strength of intervention applied to elderly, between 0 and 1 : numeric
#' @param T3_start : start of intervention applied to elderly : numeric
#' @param T3_end : end of intervention applied to elderly : numeric
#' @param tmax :  maximum simulation time : numeric
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver.
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#'   The model implement basic processes of infection, recovery and death.
#'   Waning immunity is also implemented.
#'   Control is applied, which reduces transmission by the indicated proportion, during times tstart and tend.
#'   Control can be applied at different levels to the different groups.
#' @section Warning:
#'   This function does not perform any error checking. So if you try to do
#'   something nonsensical (e.g. any negative values or fractions > 1),
#'   the code will likely abort with an error message.
#' @examples
#'   # To run the simulation with default parameters just call the function:
#'   result <- simulate_idcontrolmultigroup_ode()
#' @author Andreas Handel
#' @export

simulate_idcontrolmultigroup_ode <- function(Sc = 1000, Ic = 0, Sa = 1000, Ia = 1, Se = 1000, Ie = 0,
                                             bcc = 0.0003, bca = 0.0001, bce = 0.0001, bac = 0.0001, baa = 0.0003, bae = 0.0001, bec = 0.0001, bea = 0.0001, bee = 0.0003,
                                             gc = 0.1, ga = 0.1, ge = 0.1, wc = 0, wa = 0, we = 0, mc = 0.001, ma = 0.01, me = 0.1,
                                             f1 = 0, T1_start = 50, T1_end = 150, f2 = 0, T2_start = 50, T2_end = 150, f3 = 0, T3_start = 50, T3_end = 150,  tmax = 600)
{

  # This function is used in the solver function and has no independent usages
  interventionmodel <- function(t, y, parms)
  {
    with(
      as.list(c(y,parms)), #lets us access variables and parameters stored in y and pars by name
      {

        #apply intervention, which reduces rates at which a group gets infected
        #since bac means transmission from adult to children, intervention for kids would reduce bac (but not bca)
        if (t>=T1_start &&  t<=T1_end) {bcc = (1 - f1) * bcc; bca = (1 - f1) * bca; bce = (1 - f1) * bce; }
        if (t>=T2_start &&  t<=T2_end) {bac = (1 - f2) * bac; baa = (1 - f2) * baa; bae = (1 - f2) * bae; }
        if (t>=T3_start &&  t<=T3_end) {bec = (1 - f3) * bec; bea = (1 - f3) * bea; bee = (1 - f3) * bee; }

        #the ordinary differential equations
        dSc =  - Sc * (bcc * Ic + bca * Ia + bce * Ie) + wc * Rc
        dIc =    Sc * (bcc * Ic + bca * Ia + bce * Ie) - gc * Ic
        dRc =   (1-mc)*gc * Ic - wc * Rc
        dDc =   mc*gc*Ic

        dSa =  - Sa * (bac * Ic + baa * Ia + bae * Ie) + wa * Ra
        dIa =    Sa * (bac * Ic + baa * Ia + bae * Ie) - ga * Ia
        dRa =   (1-ma)*ga * Ia - wa * Ra
        dDa =   ma*ga*Ia

        dSe =  - Se * (bec * Ic + bea * Ia + bee * Ie) + we *Re
        dIe =    Se * (bec * Ic + bea * Ia + bee * Ie) - ge * Ie
        dRe =   (1-me)*ge * Ie - we * Re
        dDe =   me*ge*Ie

        list(c(dSc, dIc, dRc, dDc, dSa, dIa, dRa, dDa, dSe, dIe, dRe, dDe))
      }
    ) #close with statement
  } #end function specifying the ODEs

  ############################################################
  Y0 = c(Sc = Sc, Ic = Ic, Rc = 0, Dc = 0, Sa = Sa, Ia = Ia, Ra = 0, Da = 0, Se = Se, Ie = Ie, Re = 0, De = 0);  #combine initial conditions into a vector
  dt = min(0.5, tmax / 100); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

  ############################################################
  #vector of parameters which is sent to the ODE function
  pars=c(bcc = bcc, bca = bca, bce = bce, bac = bac, baa = baa, bae = bae , bec = bec, bea = bea, bee = bee, gc = gc, ga = ga, ge = ge, wc = wc, wa = wa, we = we, mc = mc, ma = ma, me = me,
         f1  = f1, T1_start = T1_start, T1_end = T1_end, f2 = f2, T2_start = T2_start, T2_end = T2_end, f3 = f3, T3_start = T3_start, T3_end = T3_end)

  odeoutput = deSolve::ode(y = Y0, times = timevec, func = interventionmodel, parms=pars, method = "lsoda", atol=1e-8, rtol=1e-8);

  result <- list()
  result$ts <- as.data.frame(odeoutput)
  return(result)
}
