#' Stochastic simulation of an SEIR-type model
#' 
#' @description  Simulation of a stochastic SEIR type model with the following
#'   compartments: Susceptibles (S), Infected and pre-symptomatic/exposed (E), 
#'   Infected and Symptomatic (I), Recovered and Immune (R)
#'   
#' @param S : initial number of susceptible hosts : numeric
#' @param I : initial number of infected, symptomatic hosts : numeric
#' @param bE : level/rate of infectiousness for hosts in the E compartment : numeric
#' @param bI : level/rate of infectiousness for hosts in the I compartment : numeric
#' @param gE : rate at which a person leaves the E compartment : numeric
#' @param gI : rate at which a person leaves the I compartment : numeric
#' @param w : rate at which recovered lose immunity and return to susceptible : numeric
#' @param m : the rate at which new individuals enter the model (are born) : numeric
#' @param n : the rate of natural death (the inverse is the average lifespan) : numeric
#' @param tmax : maximum simulation time : numeric
#' @param rngseed : seed for random number generator to allow reproducibility : numeric
#' @return The function returns a list. The list has one element, a data frame ts
#' which contains the time series of the simulated model, 
#' with one column per compartment/variable. The first column is time.
#' @details A compartmental ID model with several states/compartments is
#'   simulated. Initial conditions for the E and R variables are 0. Units of
#'   time depend on the time units chosen for model parameters. The simulation
#'   runs as a stochastic model using the adaptive-tau algorithm as implemented
#'   by ssa.adaptivetau() in the adpativetau package. See the manual of this
#'   package for more details. 
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message.
#' @examples
#' # To run the simulation with default parameters, just call the function:
#' result <- simulate_seir_stochastic()
#' # To choose parameter values other than the standard one, specify them like this:
#' result <- simulate_seir_stochastic(S = 2000,  tmax = 200, bE = 0.01)
#' # You can display or further process the result, like this:
#' plot(result$ts[,'time'],result$ts[,'S'],xlab='Time',ylab='Number Susceptible',type='l')
#' print(paste('Max number of infected: ',max(result$ts[,'I']))) 
#' @seealso See the Shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the adaptivetau
#' package for details on the stochastic algorithm.
#' @author Andreas Handel
#' @export


simulate_seir_stochastic <- function(S = 1000, I = 10, bE = 0, bI = 1e-3, gE = 0.5, gI = 0.5, w = 0, m = 0, n = 0, tmax = 100, rngseed = 100)
{
 
    #this specifies the rates used by the adapativetau routine
    stochasticSEIRfunc <- function(y, parms, t)
    {
        with(as.list(c(y, parms)),
             {
                 #specify each rate/transition/reaction that can happen in the system
                 rates=c(  m,
                           n * S,
                           n * E,
                           n * I,
                           n * R,
                           S * bE * E,
                           S * bI * I,
                           gE * E,
                           gI * I,
                           w * R
                 ) #end specification of each rate/transition/reaction
                 return(rates)
             })
    } #end function specifying rates used by adaptivetau
    
    
    Y0 = c(S = S, E = 0,  I = I, R = 0);  #combine initial conditions into a vector
    dt = tmax / 1000; #time step for which to get results back
    timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

    #combining parameters into a parameter vector
    pars = c(bE = bE, bI = bI, gE = gE,  gI = gI, w = w, m = m, n = n);

    #specify for each reaction/rate/transition how the different variables change
    #needs to be in exactly the same order as the rates listed in the rate function
    transitions = list(c(S = +1), #births of susceptible
                       c(S = -1), #deaths of susceptible
                       c(E = -1), #deaths of E
                       c(I = -1), #deaths of I
                       c(R = -1), #deaths of R
                       c(S = -1, E = +1), #infection of S by E
                       c(S = -1, E = +1), #infection of S by I
                       c(E = -1, I = +1), #move of E to I (becoming symptomatic)
                       c(I = -1, R = +1), #move of I to R (recovery)
                       c(R = -1, S = +1) #move of R to S (waning immunity)
    ) #end list of transitions



    #this line runs the simulation using the SSA algorithm in the adaptivetau package
    set.seed(rngseed) # to allow reproducibility
    simres = adaptivetau::ssa.adaptivetau(init.values = Y0, transitions = transitions,  rateFunc = stochasticSEIRfunc, params = pars, tf = tmax)
    result <- list()
    result$ts <- as.data.frame(simres)
    
    return(result)
}
