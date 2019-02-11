#' Stochastic simulation of a compartmental SIR-type model with wild-type and mutant strains and treatment
#'
#' @description  Simulation of a stochastic 2-strain SIR model with the following compartments:
#' Susceptibles (S), Infected with wild-type/sensitive and untreated (Iu),
#' Infected with wild-type and treated (It), infected with resistant (Ir), 
#' Recovered and Immune (R). allows exploration of evolutionary dynamics
#' @details A compartmental ID model with several states/compartments
#' is simulated as a stochastic model using the adaptive tau algorithm as implemented by ssa.adaptivetau()
#' in the adaptivetau package. See the manual of this package for more details. 
#' @param S : initial number of susceptible hosts : numeric
#' @param Iu : initial number of wild-type infected untreated hosts : numeric
#' @param It : initial number of wild-type infected treated hosts : numeric
#' @param Ir : initial number of resistant infected hosts : numeric
#' @param bu : level/rate of infectiousness for hosts in the Iu compartment : numeric
#' @param bt : level/rate of infectiousness for hosts in the It compartment : numeric
#' @param br : level/rate of infectiousness for hosts in the Ir compartment : numeric
#' @param cu : fraction of resistant mutant infections that an untreated host produces : numeric
#' @param ct : fraction of resistant mutant infections that a treated host produces : numeric
#' @param f : fraction of infected receiving treatment : numeric
#' @param gu : rate at which a host leaves the Iu compartment : numeric
#' @param gt : rate at which a person leaves the It compartment : numeric
#' @param gr : rate at which a person leaves the Ir compartment : numeric
#' @param tmax : maximum simulation time : numeric
#' @param rngseed : seed for random number generator to allow reproducibility : numeric
#' @return This function returns the simulation result as obtained from a call
#'   to the adaptivetau integrator in list form. The list element ts is a
#'   dataframe where the first column is "time," and the remaining columns are the variables
#' @section Warning:
#' This function does not perform any error checking. So if you try to do
#' something nonsensical (e.g. have negative values or fractions > 1),
#' the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call the function:
#' result <- simulate_evolution_stochastic()
#' # To choose parameter values other than the standard one, specify them like such:
#' result <- simulate_evolution_stochastic(S = 2000,  tmax = 200, bt = 0.01)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$ts[ , "time"],result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')
#' @references See the manual for the adaptivetau package for details on the algorithm.
#'             The implemented model is loosely based on: Handel et al 2009 JTB 
#'            "Antiviral resistance and the control of pandemic influenza: The roles of
#'            stochasticity, evolution and model details"
#' @author Andreas Handel
#' @export


simulate_evolution_stochastic <- function(S = 1000, Iu = 1, It = 1, Ir = 1, bu = 1e-3, bt = 1e-3, br = 1e-3, cu = 1e-3, ct = 1e-2, f = 0, gu = 1, gt = 1, gr = 1, tmax = 100 ,rngseed = 100)
{
    
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
    
    
    
    
    Y0 = c(S = S, Iu = Iu,  It = It, Ir = Ir, R = 0);  #combine initial conditions into a vector
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
    set.seed(rngseed) # to allow reproducibility
    simres <- adaptivetau::ssa.adaptivetau(init.values = Y0, transitions = transitions,  rateFunc = evolutionratefunc, params = pars, tf = tmax)

    result <- list()
    result$ts <- as.data.frame(simres)
    
    return(result)
}
