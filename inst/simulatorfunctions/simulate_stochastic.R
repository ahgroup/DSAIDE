############################################################
##simulating a stochastic SEIR type model
##written by Andreas Handel, ahandel@uga.edu, last change: 10/18/16
############################################################

#this specifies the rates used by the adapativetau routine
stochasticratefunc <- function(y, parms, t)
{
    with(as.list(c(y, parms)),
         {
            #specify each rate/transition/reaction that can happen in the system
             rates=c(  lambda,
                       n * S,
                       n * P,
                       n * I,
                       n * R,
                       S * bP * P,
                       S * bI * I,
                       gP * P,
                       gI * I,
                       w * R
             ) #end specification of each rate/transition/reaction
             return(rates)
         })
} #end function specifying rates used by adaptivetau


#' Stochastic simulation of an SEIR-type model
#' 
#' @description  Simulation of a stochastic SEIR type model with the following
#'   compartments: Susceptibles (S), Infected and pre-symptomatic (P), 
#'   Infected and Symptomatic (I), Recovered and Immune (R)
#'   
#' @param S0 initial number of susceptible hosts
#' @param I0 initial number of infected, symptomatic hosts
#' @param bP level/rate of infectiousness for hosts in the P compartment
#' @param bI level/rate of infectiousness for hosts in the I compartment
#' @param gP rate at which a person leaves the P compartment, which is the
#'   inverse of the average time spent in that compartment
#' @param gI rate at which a person leaves the I compartment
#' @param w rate at which recovered persons loose immunity and return to
#'   susceptible state
#' @param lambda the rate at which new individuals enter the model (are born)
#' @param n the rate of natural death (the inverse is the average lifespan)
#' @param tmax maximum simulation time, units depend on choice of units for 
#' your parameters
#' @return The function returns the time series of the simulated model as
#'   matrix, with one column per compartment/variable. The first column is time.
#' @details A compartmental ID model with several states/compartments is
#'   simulated. Initial conditions for the P and R variables are 0. Units of
#'   time depend on the time units chosen for model parameters. The simulation
#'   runs as a stochastic model using the adaptive-tau algorithm as implemented
#'   by ssa.adaptivetau #' in the adpativetau package. See the manual of this
#'   package for more details. The function returns the time series of the
#'   simulated disease as output matrix, with one column per
#'   compartment/variable. The first column is time.
#' @section Warning: This function does not perform any error checking. So if
#'   you try to do something nonsensical (e.g. specify negative parameter values
#'   or fractions > 1), the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters 
#' result <- simulate_stochastic()
#' # To choose parameter values other than the standard one, specify them e.g. like this
#' result <- simulate_stochastic(S0 = 2000,  tmax = 200, bP = 1/100)
#' 
#' # You can display or further process the result, e.g. like this
#' plot(result[,'time'],result[,'S'],xlab='Time',ylab='Number Susceptible',type='l')
#' print(paste('Total number of infected at end of simulation:',result[nrow(result),'R'])) 
#' @seealso See the shiny app documentation corresponding to this simulator
#' function for more details on this model. See the manual for the adaptivetau
#' package for details on the stochastic algorithm.
#' @author Andreas Handel
#' @export




simulate_stochastic <- function(S0 = 1000, I0 = 10, tmax = 100, bP = 0, bI = 1/1000, gP = 0.5, gI = 0.5, w = 0, lambda = 0, n = 0)
{
    Y0 = c(S = S0, P = 0,  I = I0, R = 0);  #combine initial conditions into a vector
    dt = tmax / 1000; #time step for which to get results back
    timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

    #combining parameters into a parameter vector
    pars = c(bP = bP, bI = bI, gP = gP,  gI = gI, w = w, lambda = lambda, n = n);

    #specify for each reaction/rate/transition how the different variables change
    #needs to be in exactly the same order as the rates listed in the rate function
    transitions = list(c(S = +1), #births of susceptible
                       c(S = -1), #deaths of susceptible
                       c(P = -1), #deaths of P
                       c(I = -1), #deaths of I
                       c(R = -1), #deaths of R
                       c(S = -1, P = +1), #infection of S by P
                       c(S = -1, P = +1), #infection of S by I
                       c(P = -1, I = +1), #move of P to I (becoming symptomatic)
                       c(I = -1, R = +1), #move of I to R (recovery)
                       c(R = -1, S = +1) #move of R to S (waning immunity)
    ) #end list of transitions



    #this line runs the simulation, i.e. integrates the differential equations describing the infection process
    #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
    output = adaptivetau::ssa.adaptivetau(init.values = Y0, transitions = transitions,  rateFunc = stochasticratefunc, params = pars, tf = tmax)

    #since I use P above, but it's better to rename as SEIR in the plots, do a rename here
    colnames(output)[3]<-"E"
    
    #The output produced by a call to the odesolver is odeoutput matrix is returned by the function
    return(output)
}
