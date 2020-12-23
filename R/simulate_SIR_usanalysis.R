#' Simulation to illustrate uncertainty and sensitivity analysis
#'
#' @description This function performs uncertainty and sensitivity analysis
#' using the SIRS model.
#' @details The SIRS model with demographics
#' is simulated for different parameter values.
#' The user provides ranges for the initial conditions and parameter values and the number of samples.
#' The function does Latin Hypercube Sampling (LHS) of the parameters
#' and runs the model for each sample.
#' Distribution for all parameters is assumed to be uniform between the min and max values.
#' The only exception is the recovery parameter,
#' which (for illustrative purposes) is assumed to be
#' gamma distributed with the specified mean and variance.
#' This code is part of the DSAIDE R package.
#' For additional model details, see the corresponding app in the DSAIDE package.
#' @param Smin : lower bound for initial susceptible : numeric
#' @param Smax : upper bound for initial susceptible : numeric
#' @param Imin : lower bound for initial infected : numeric
#' @param Imax : upper bound for initial infected : numeric
#' @param bmin : lower bound for infection rate : numeric
#' @param bmax : upper bound for infection rate : numeric
#' @param gmean : mean for recovery rate : numeric
#' @param gvar : variance for recovery rate : numeric
#' @param nmin : lower bound for birth rate : numeric
#' @param nmax : upper bound for birth rate : numeric
#' @param mmin : lower bound for death rate : numeric
#' @param mmax : upper bound for death rate : numeric
#' @param wmin : lower bound for waning immunity rate : numeric
#' @param wmax : upper bound for waning immunity rate : numeric
#' @param samples : number of LHS samples to run : numeric
#' @param rngseed : seed for random number generator : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : times for which result is returned : numeric
#' @return The function returns the output as a list.
#' The list element 'dat' contains a data frame.
#' The simulation returns for each parameter sample the peak and final value for I and final for S.
#' Also returned are all parameter values as individual columns
#' and an indicator stating if steady state was reached.
#' A final variable 'steady' is returned for each simulation.
#' It is TRUE if the simulation did reach steady state, otherwise FALSE.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters just call the function:
#' \dontrun{result <- simulate_SIR_usanalysis()}
#' # To choose parameter values other than the standard one, specify them, like such:
#' result <- simulate_SIR_usanalysis(gmean = 2, gvar = 0.2, samples = 5, tfinal = 50)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$dat[,"g"],result$dat[,"Ipeak"],xlab='values for g',ylab='Peak Bacteria',type='l')
#' @seealso See the Shiny app documentation corresponding to this simulator
#' function for more details on this model.
#' @author Andreas Handel
#' @export


simulate_SIR_usanalysis <- function(Smin = 1000, Smax = 1000, Imin = 10, Imax = 10, bmin=0.005, bmax=0.01, gmean=0.5, gvar=0.01, nmin = 0, nmax = 0, mmin = 0, mmax = 0, wmin = 0, wmax = 0, samples = 5, rngseed = 100, tstart = 0, tfinal = 500, dt = 0.1)
  {

    #this creates a LHS with the specified number of samples for all parameters
    #drawn from a uniform distribution between zero and one
    #if a parameter should be kept fixed, simply set min and max to the same value
    set.seed(rngseed)
    lhssample=lhs::randomLHS(samples,7);

    #transforming parameters to be  uniform between their low and high values
    Svec = stats::qunif(lhssample[,1],min = Smin, max = Smax)
    Ivec = stats::qunif(lhssample[,2],min = Imin, max = Imax)
    bvec = stats::qunif(lhssample[,3],min = bmin, max = bmax)
    nvec = stats::qunif(lhssample[,4],min = nmin, max = nmax)
    mvec = stats::qunif(lhssample[,5],min = mmin, max = mmax)
    wvec = stats::qunif(lhssample[,6],min = wmin, max = wmax)

    #transforming parameter g to a gamma distribution with mean gmean and variance gvar
    #this is just to illustrate how different assumptions of parameter distributions can be implemented
    gvec = stats::qgamma(lhssample[,7], shape = gmean^2/gvar, scale = gvar/gmean);

    Ipeak=rep(0,samples) #initialize vectors that will contain the solution
    Ifinal=rep(0,samples)
    Sfinal=rep(0,samples)

    steady = rep(TRUE,samples) #indicates if steady state has not been reached
    for (ct in 1:samples)
    {
        #values for sampled parameters
        S=Svec[ct]
        I=Ivec[ct]
        b=bvec[ct]
        g=gvec[ct]
        m=mvec[ct]
        n=nvec[ct]
        w=wvec[ct]

        #this runs the bacteria ODE model for each parameter sample
        #all other parameters remain fixed
        odeout <- simulate_SIRSd_model_ode(S = S, I = I, R = 0, b = b, g = g, n = n, m = m, w = w, tstart = tstart, tfinal = tfinal, dt = dt)

        timeseries = odeout$ts

        Ipeak[ct] = max(timeseries[,"I"]); #get the peak for I
        Ifinal[ct] = utils::tail(timeseries[,"I"],1)
        Sfinal[ct] = utils::tail(timeseries[,"S"],1)

        #if any of the recorded results is nonsensical, return as NA
        if ( !is.finite(Ipeak[ct])  ) { Ipeak[ct] <- NA}
        if ( !is.finite(Ifinal[ct]) ) { Ifinal[ct] <- NA}
        if ( !is.finite(Sfinal[ct]) ) { Sfinal[ct] <- NA}

        #a quick check to make sure the system is at steady state,
        #i.e. the value for I at the final time is not more than
        #1% different than I several time steps earlier
        #add a tiny amount to denominator to avoid division by 0
        vl=nrow(timeseries)
        finaldiff = (abs(timeseries[vl,"I"]-timeseries[vl-10,"I"])/(1e-12 + timeseries[vl,"I"]))
        if (is.nan(finaldiff) || is.na(finaldiff)) { finaldiff = 1} #if something isn't going right, set to 'non-steady state'
        if (abs(finaldiff) > 1e-2)
        {
          steady[ct] = FALSE
        }
    }

    simresults = data.frame(Ipeak = Ipeak, Ifinal = Ifinal, Sfinal = Sfinal, S = Svec, I = Ivec, b = bvec, g = gvec, n = nvec, m = mvec, w = wvec, steady = steady)

    result = list()
    result$dat = simresults
    return(result)
}
