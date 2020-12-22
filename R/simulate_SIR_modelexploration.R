#' Simulation to illustrate parameter scan of the basic SIR model with births and deaths
#'#'
#' @description This function simulates the SIRS model ODE for a range of parameters.
#' The function returns a data frame containing the parameter that has been varied and the outcomes (see details).
#'
#' @param S : starting value for Susceptible : numeric
#' @param I : starting value for Infected : numeric
#' @param R : starting value for Recovered : numeric
#' @param b : infection rate : numeric
#' @param g : recovery rate : numeric
#' @param w : rate of waning immunity : numeric
#' @param n : the rate at which new individuals enter the model (are born) : numeric
#' @param m : the rate of natural death (the inverse is the average lifespan) : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : Times for which result is returned : numeric
#' @param samples : Number of values to run between pmin and pmax : numeric
#' @param parmin : Lower value for varied parameter : numeric
#' @param parmax : Upper value for varied parameter : numeric
#' @param samplepar : Name of parameter to be varied : character
#' @param pardist : spacing of parameter values, can be either 'lin' or 'log' : character
#' @return The function returns the output as a list,
#' list element 'dat' contains the data frame with results of interest.
#' The first column is called xvals and contains the values of the
#' parameter that has been varied as specified by 'samplepar'.
#' The remaining columns contain maximum and final state numbers of susceptible, infected and recovered
#' Smax, Imax, Rmax and Sfinal, Ifinal, Rfinal.
#' A final boolean variable 'steady' is returned for each simulation.
#' It is TRUE if the simulation reached steady state, otherwise FALSE.
#' @details This code illustrates how to systematically analyze the impact of a specific parameter.
#' The SIR ODE model with births and deaths is simulated for different parameter values.
#' The user can specify which parameter is sampled, and
#' the simulation returns for each parameter sample the max and final value for the variables.
#' Also returned is the varied parameter and an indicator if steady state was reached.
#' @section Notes: The parameter dt only determines for which times the solution is returned,
#' it is not the internal time step. The latter is set automatically by the ODE solver.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' \dontrun{res <- simulate_SIR_modelexploration()}
#' # To choose parameter values other than the standard one, specify them, like such:
#' res <- simulate_SIR_modelexploration(tfinal=100, samples=5, samplepar='g', parmin=0.1, parmax=1)
#' # You should then use the simulation result returned from the function, like this:
#' plot(res$dat[,"xvals"],res$data[,"Imax"],xlab='Parameter values',ylab='Max Infected',type='l')
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model.
#' @author Andreas Handel
#' @export


simulate_SIR_modelexploration <- function(S = 1000, I = 1, R = 0, b = 0.002, g = 1, w = 0, n = 0, m = 0, tstart = 0, tfinal = 1000, dt = 0.1, samples = 10, parmin=0.0005, parmax=0.005, samplepar='b',  pardist = 'lin')
  {

    #initialize vectors that will contain the outcomes of interest
    Smax=rep(0,samples)
    Imax=rep(0,samples)
    Rmax=rep(0,samples)
    Sfinal=rep(0,samples)
    Ifinal=rep(0,samples)
    Rfinal=rep(0,samples)

    #create values for the parameter of interest to sample over
    #do equal spacing in log space
    if (pardist == 'lin') {parvec=seq(parmin,parmax,length=samples)}
    if (pardist == 'log') {parvec=10^seq(log10(parmin),log10(parmax),length=samples)}


    steady = rep(TRUE,samples) #indicates if steady state has not been reached
    for (ct in 1:samples)
    {
        #replace value of parameter we want to vary
        if (samplepar == 'b') {b = parvec[ct]}
        if (samplepar == 'g') {g = parvec[ct]}
        if (samplepar == 'm') {m = parvec[ct]}
        if (samplepar == 'n') {n = parvec[ct]}
        if (samplepar == 'w') {w = parvec[ct]}


        #this runs the bacteria ODE model for each parameter sample
        #all other parameters remain fixed
        odeout <- simulate_SIRSd_model_ode(S = S, I = I, R = R, b = b, g = g, w = w, n = n, m = m, tstart = tstart, tfinal = tfinal, dt = dt)

        timeseries = odeout$ts

        Smax[ct] = max(timeseries[,"S"])
        Imax[ct] = max(timeseries[,"I"])
        Rmax[ct] = max(timeseries[,"R"])
        Sfinal[ct] = utils::tail(timeseries[,"S"],1)
        Ifinal[ct] = utils::tail(timeseries[,"I"],1)
        Rfinal[ct] = utils::tail(timeseries[,"R"],1)


        #a quick check to make sure the system is at steady state.
        #If number infected are not negligible
        #and the value for R at the final time is more than
        #0.1% different than R several time steps earlier
        #we assume steady state was not reached
        vl=nrow(timeseries);
        if ( (abs(timeseries[vl,"I"]-timeseries[vl-10,"I"])/abs(timeseries[vl,"I"]) )>1e-3 && abs(timeseries[vl,"I"])>1e-10)
        {
          steady[ct] = FALSE
        }
    }

    #final list structure containing all results that are returned
    result = list()
    dat = data.frame(xvals = parvec, Smax = Smax, Imax = Imax, Rmax = Rmax, Sfinal = Sfinal, Ifinal = Ifinal, Rfinal = Rfinal, steady = steady)
    result$dat = dat

    return(result)
}
