#' Basic discrete time SIR model
#' 
#' @description A basic SIR model with 3 compartments and infection and recovery processes
#' 
#' @details The model includes susceptible, infected, and recovered compartments. The two processes that are modeled are infection and recovery. The model is implemented as a discrete-time simulation.
#' @param S : starting value for Susceptible : numeric
#' @param I : starting value for Infected : numeric
#' @param R : starting value for Recovered : numeric
#' @param b : infection rate : numeric
#' @param g : recovery rate : numeric
#' @param tstart : Start time of simulation : numeric
#' @param tfinal : Final time of simulation : numeric
#' @param dt : Time step : numeric
#' @return The function returns the output as a list. 
#' The time-series from the simulation is returned as a dataframe saved as list element \code{ts}. 
#' The \code{ts} dataframe has one column per compartment/variable. The first column is time.   
#' @examples  
#' # To run the simulation with default parameters:  
#' result <- simulate_sir_discrete() 
#' @section Warning: This function does not perform any error checking. So if you try to do something nonsensical (e.g. have negative values for parameters), the code will likely abort with an error message.
#' @export 
 
simulate_sir_discrete <- function(S = 1000, I = 1, R = 0, b = 0.002, g = 1, tstart = 0, tfinal = 100, dt = 0.1) 
{ 
  #Function that encodes simulation loop 
  SIR_model_discrete <- function(vars, pars, times) 
  {
    with( as.list(c(vars,pars,times)), {  
      tvec = seq(tstart,tfinal,by=dt) 
      ts = data.frame(cbind(tvec, matrix(0,nrow=length(tvec),ncol=length(vars)))) 
      colnames(ts) = c("time","S","I","R") 
      ct=1 #a counter to index array 
      for (t in tvec) 
      {
        ts[ct,] = c(t,S,I,R) 
        Sp = S + dt*(-b*S*I) 
        Ip = I + dt*(+b*S*I -g*I) 
        Rp = R + dt*(+g*I) 
        S = Sp 
        I = Ip 
        R = Rp 
        ct = ct + 1 
      } #finish loop 
      return(ts) 
    }) #close with statement 
  } #end function encoding block 
  

  #Main function code block 
  vars = c(S = S, I = I, R = R)
  pars = c(b = b, g = g)
  times = c(tstart = tstart, dt = dt, tfinal = tfinal)
  ts <- SIR_model_discrete(vars = vars, pars = pars, times = times) 
  result <- list() 
  result$ts <- ts 
  return(result) 
} 


