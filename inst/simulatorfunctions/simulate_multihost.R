# This function is used in the solver function and has no independent usages
multieq <- function(t, y, parms)
{
  with(
    as.list(c(y, parms)), #lets us access variables and parameters stored in y and pars by name
    {
      
      dS <- -S*(b1*I1 + b2*I2 + b12*I12)
      dI1 <- (b1*I1 + a*b12*I12)*S - g1*I1 - b12*I1*I12
      dI2 <- (b2*I2 + (1 - a)*b12*I12)*S - g2*I2 - b12*I2*I12
      dI12 <- b12*I12*(I1 + I2 + R1 + R2) - g12*I12
      dR1 <- g1*I1 - b12*R1*I12
      dR2 <- g2*I2 - b12*R2*I12
      dR12 <- g12*I12
      
      
      list(c(dS, dI1, dI2, dI12, dR1, dR2, dR12))
    }
  ) #close with statement
} #end function specifying the ODEs

simulate_multi <- function(S = 1e3, I10 = 1, I20 = 0, I12 = 0, R1 = 0, R2 = 0, R12 = 0, tmax = 120, a = 0, b1 = 0, b2 = 0, b12 = 0, g1 = 1, g2 = 1, g12 = 1)
{
  ############################################################
  Y0 = c(S = S, I1 = I10, I2 = I20, I12 = I12, R1 = R1, R2 = R2, R12 = R12);  #combine initial conditions into a vector
  dt = min(0.1, tmax / 1000); #time step for which to get results back
  timevec = seq(0, tmax, dt); #vector of times for which solution is returned (not that internal timestep of the integrator is different)
  
  
  ############################################################
  #vector of parameters which is sent to the ODE function  
  pars <- c(a = a, b1 = b1, b2 = b2, b12 = b12, g1 = g1, g2 = g2, g12 = g12);
  
  #this line runs the simulation, i.e. integrates the differential equations describing the infection process
  #the result is saved in the odeoutput matrix, with the 1st column the time, the 2nd, 3rd, 4th column the variables S, I, R
  #This odeoutput matrix will be re-created every time you run the code, so any previous results will be overwritten
  odeoutput = deSolve::lsoda(Y0, timevec, func = multieq, parms=pars, atol=1e-12, rtol=1e-12);
  
  return (odeoutput)
}



