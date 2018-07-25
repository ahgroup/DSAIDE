# This function specifies and runs the ODE model for this app/simulation
# For an explanation of the model, see the documentation for this app
multipatheq <- function(t, y, parms)
{
    with(
        as.list(c(y, parms)), #lets us access variables and parameters stored in y and pars by name
        {
            
            dS <- -S*(b1*(I1+I1X) + b2*(I2+I2X) + b12*I12)
            
            dI1 <- (b1*(I1+I1X) + a*b12*I12)*S       - g1*I1 - (b2*(I2+I2X) + b12*I12)*I1
            dI2 <- (b2*(I2+I2X) + (1 - a)*b12*I12)*S - g2*I2 - (b1*(I1+I1X) + b12*I12)*I2
            dI12 <- (b1*(I1+I1X) + b12*I12)*I2 + (b2*(I2+I2X) + b12*I12)*I1 - g12*I12 

            dI1X <- (b1 * (I1 + I1X) + b12*I12)*R2 - g1*I1X
            dI2X <- (b2 * (I2 + I2X) + b12*I12)*R1 - g2*I2X
            
            dR1 <- g1*I1 - (b2 * (I2 + I2X) + b12*I12)*R1
            dR2 <- g2*I2 - (b1 * (I1 + I1X) + b12*I12)*R2
            
            dR12 <- g12*I12 + g1*I1X + g2*I2X
            
            
            list( c(dS, dI1, dI2, dI12, dI1X, dI2X, dR1, dR2, dR12))        
          }
    ) #close with statement
} #end function specifying the ODEs

#' Simulation of a compartmental infectious disease transmission model with 2 types of pathogens
#'
#' @description  This model allows for the simulation of 2 IDs in a single host
#' 
#'
#' @param S0 initial number of susceptible hosts 
#' @param I10 initial number of hosts infected with type 1
#' @param I20 initial number of hosts infected with type 2
#' @param I120 initial number of double infected hosts
#' @param tmax maximum simulation time, units of months
#' @param b1 rate at which type 1 infected hosts transmit
#' @param b2 rate at which type 1 infected hosts transmit
#' @param b12 rate at which double infected hosts transmit
#' @param g1 the rate at which infected type 1 hosts recover
#' @param g2 the rate at which infected type 2 hosts recover
#' @param g12 the rate at which double infected hosts recover
#' @param a fraction of type 1 infections produced by double infected hosts
#' @return This function returns the simulation result as obtained from a call
#'   to the deSolve ode solver
#' @details A compartmental ID model with several states/compartments
#'   is simulated as a set of ordinary differential
#'   equations. The function returns the output from the odesolver as a matrix,
#'   with one column per compartment/variable. The first column is time.
#' @section Warning:
#'   This function does not perform any error checking. So if you try to do
#'   something nonsensical (e.g. any negative values or fractions > 1),
#'   the code will likely abort with an error message
#' @examples
#'   # To run the simulation with default parameters just call this function
#'   result <- simulate_multipathogen()
#'   # To choose parameter values other than the standard one, specify them e.g. like such
#'   result <- simulate_multipathogen(S0 = 100, I20 = 10,  tmax = 100, a = 0.5)
#'   # You should then use the simulation result returned from the function, e.g. like this:
#'   plot(result$ts[ , "Time"], result$ts[ , "S"], xlab='Time', ylab='Number Susceptible', type='l')
#'   # We could set the infected type 1 host recovery rate at a high level, e.g., 1.2, and
#'   # examine the infected 1 curve.
#'   result <- simulate_multipathogen(S0 = 100, I20 = 10, tmax = 100, g1 = 1.2)
#'   plot(result$ts[ , "Time"], result$ts[ , "I1"], xlab = "Time", ylab = "Number Infected Type 1", type = "l")
#'   # Additionally, consider making type 1 hosts transmit at a high rate.
#'   result <- simulate_multipathogen(S0 = 100, I20 = 10, tmax = 100, b1 = 2.5)
#'   plot(result$ts[ , "Time"], result$ts[ , "I1"], xlab = "Time", ylab = "Number Infected Type 1", type = "l")
#' @seealso The UI of the shiny app 'Multi-Pathogen Dynamics', which is part of this package, contains more details on the model
#' @author Andreas Handel and Spencer Hall
#' @references See e.g. Keeling and Rohani 2008 for SIR models and the
#'   documentation for the deSolve package for details on ODE solvers
#' @export


simulate_multipathogen <- function(S0 = 1e3, I10 = 1, I20 = 0, I120 = 0, tmax = 120, b1 = 1e-3, b2 = 0, b12 = 0, g1 = 1, g2 = 1, g12 = 1, a = 0)
{
    ############################################################
    Y0 = c(S = S0, I1 = I10, I2 = I20, I12 = I120, I1X = 0 , I2X = 0, R1 = 0, R2 = 0, R12 = 0);  #combine initial conditions into a vector
    dt = min(0.1, tmax / 1000); #time step for which to get results back
    timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)
    
    
    ############################################################
    #vector of parameters which is sent to the ODE function  
    pars <- c(b1 = b1, b2 = b2, b12 = b12, g1 = g1, g2 = g2, g12 = g12, a = a);
    
    #this line runs the simulation, i.e. integrates the differential equations describing the infection process
    #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
    #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
    odeoutput = deSolve::lsoda(Y0, timevec, func = multipatheq, parms=pars, atol=1e-12, rtol=1e-12);
    
    colnames(odeoutput) <- c("Time","S","I1","I2",'R1','R2',"I1X","I2X","I12",'R12')
    result <- list()
   # result$ts1 <- as.data.frame(odeoutput[ , c(1, 2:6)])
   # result$ts2 <- as.data.frame(odeoutput[ , c(1, 7:10)])
    result$ts <- as.data.frame(odeoutput)
    
    return(result)
}