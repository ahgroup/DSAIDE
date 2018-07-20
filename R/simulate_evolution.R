############################################################
##simulating a stochastic SIR type model with 2 types
#assumption is these are wild-type and mutant
#allows exploration of evolutionary dynamics
##written by Andreas Handel, ahandel@uga.edu, last change: 10/6/16
############################################################

#this specifies the rates used by the adapativetau routine
evolutionratefunc <- function(y, parms, t)
{
    with(as.list(c(y, parms)),
         {
           
             #specify each rate/transition/reaction that can happen in the system
             rates=c(  (1-f) * (bu*(1-cu)*Iu + bt*(1-ct)*It) * S,
                           f * (bu*(1-cu)*Iu + bt*(1-ct)*It) * S,
                       (bu*cu*Iu + bt*ct*It + br*Ir)*S,
                       gu*Iu,
                       gt*It,
                       gr*Ir
             ) #end specification of each rate/transition/reaction
             return(rates)
         })
} #end function specifying rates used by adaptivetau


#' Stochastic simulation of a compartmental SIR-type model with wild-type and mutant strains and treatment
#'
#' @description  Simulation of a stochastic 2-strain SIR model with the following compartments:
#' Susceptibles (S), Infected with wild-type/sensitive and untreated (Iu),
#' Infected with wild-type and treated (It), infected with resistant (Ir), 
#' Recovered and Immune (R)
#'
#' @param S0 initial number of susceptible hosts
#' @param Iu0 initial number of wild-type infected untreated hosts
#' @param It0 initial number of wild-type infected treated hosts
#' @param Ir0 initial number of resistant infected hosts
#' @param bu level/rate of infectiousness for hosts in the Iu compartment
#' @param bt level/rate of infectiousness for hosts in the It compartment
#' @param br level/rate of infectiousness for hosts in the Ir compartment
#' @param cu fraction of resistant mutant infections that an untreated host produces
#' @param ct fraction of resistant mutant infections that a treated host produces
#' @param f fraction of infected receiving treatment
#' @param gu rate at which a host leaves the Iu compartment, which
#'   is the inverse of the average time spent in that compartment
#' @param gt rate at which a person leaves the It compartment
#' @param gr rate at which a person leaves the Ir compartment
#' @param tmax maximum simulation time, units depend on choice of units for your
#'   parameters
#' @return This function returns the simulation result as obtained from a call
#'   to the adaptivetau integrator
#' @details A compartmental ID model with several states/compartments
#' is simulated as a stochastic model using the adaptive tau algorithm as implemented by ssa.adaptivetau
#' in the adpativetau package. See the manual of this package for more details.
#' The function returns the time series of the simulated disease as output matrix,
#' with one column per compartment/variable. The first column is time.
#' @section Warning:
#' This function does not perform any error checking. So if you try to do
#' something nonsensical (e.g. have I0 > PopSize or any negative values or fractions > 1),
#' the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call this function
#' result <- simulate_evolution()
#' # To choose parameter values other than the standard one, specify them e.g. like such
#' result <- simulate_evolution(S0 = 2000,  tmax = 200, bt = 1/100)
#' # You should then use the simulation result returned from the function, e.g. like this:
#' plot(result[,1],result[,2],xlab='Time',ylab='Number Susceptible',type='l')
#' @references See the manual for the adaptivetau package for details on the algorithm.
#'             The implemented model is loosely based on: Handel et al 2009 JTB 
#'            "Antiviral resistance and the control of pandemic influenza: The roles of
#'            stochasticity, evolution and model details"
#' @author Andreas Handel
#' @export




simulate_evolution <- function(S0 = 1000, Iu0 = 1, It0 = 1, Ir0 = 1, tmax = 100, bu = 1/1000, bt = 1/1000, br = 1/1000, cu = 1/1000, ct = 1/100, f = 0, gu = 1, gt = 1, gr = 1)
{
    Y0 = c(S = S0, Iu = Iu0,  It = It0, Ir = Ir0, R = 0);  #combine initial conditions into a vector
    dt = tmax / 1000; #time step for which to get results back
    timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)

    #combining parameters into a parameter vector
    pars = c(bu = bu, bt = bt, br = br, cu = cu, ct = ct, f = f, gu = gu,  gt = gt, gr = gr);

    #specify for each reaction/rate/transition how the different variables change
    #needs to be in exactly the same order as the rates listed in the rate function
    transitions = list(c(S = -1, Iu = +1), #infection of S to Iu
                       c(S = -1, It = +1), #infection of S to It
                       c(S = -1, Ir = +1), #infection of S to Ir
                       c(Iu = -1, R = +1), #move of Iu to R
                       c(It = -1, R = +1), #move of It to R 
                       c(Ir = -1, R = +1) #move of Ir to R 
    ) #end list of transitions



    #this line runs the simulation, i.e. integrates the differential equations describing the infection process
    #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
    result <- adaptivetau::ssa.adaptivetau(init.values = Y0, transitions = transitions,  rateFunc = evolutionratefunc, params = pars, tf = tmax)

    colnames(result) = c('xvals','S','Iu','It','Ir','R')
    output <- list()
    output$ts <- as.data.frame(result)
    print(head(output$ts))
    return(output)
}
