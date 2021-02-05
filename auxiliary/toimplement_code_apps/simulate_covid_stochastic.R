#' Stochastic simulation of a compartmental SEIR-type model for COVID-19
#'
#' @description  Simulation of a stochastic SEIR model with the following compartments:
#' Susceptibles (S), latently infected (E), infected and later diagnosed (I), infected and never diagnosed (Iu),
#' Diagnosed cases (C) and non-diagnosed recovered (Ru)
#' @details A compartmental ID model with several states/compartments
#' is simulated as a stochastic model using the pomp simulat function
#' See the manual of this package for more details.
#' multiple dummy categories for E, I and Iu are implemented to get more realistic distributions.
#' Those dummy compartments are not publicly accessible.
#' Different interventions are implemented at different times.
#' @param S : initial number of susceptible hosts : numeric
#' @param E : initial number of infected, pre-symptomatic hosts : numeric
#' @param beta_d : level/rate of infectiousness for hosts in the I compartment : numeric
#' @param beta_u : level/rate of infectiousness for hosts in the Iu compartment : numeric
#' @param beta_e : level/rate of infectiousness for hosts in the Ie compartment : numeric
#' @param beta_red_factor : factor by which intervention reduces transmission : numeric
#' @param t_int1 : time at which intervention reduces transmission : numeric
#' @param sigma : rate at which host leaves the E stage, inverse is pre-symptomatic duration : numeric
#' @param gamma_u : rate at which host goes through I stage, inverse is time to diagnosis, pre-intervention : numeric
#' @param gamma_d : rate at which host goes through I stage, inverse is time to diagnosis, post-intervention : numeric
#' @param t_int2 : time at which intervention reduces time to diagnosis : numeric
#' @param detect_frac_0 : fraction of infected individuals who will get diagnosed, pre-intervention : numeric
#' @param detect_frac_1 : fraction of infected individuals who will get diagnosed, post-intervention : numeric
#' @param t_int3 : time at which intervention reduces time to diagnosis : numeric
#' @param tmax : maximum simulation time : numeric
#' @param rngseed : seed for random number generator to allow reproducibility : numeric
#' @return This function returns the simulation result as obtained from a call
#'   to the pomp::simulate integrator in list form. The list element ts is a
#'   dataframe where the first column is "time," and the remaining columns are the variables
#' @section Warning:
#' This function does not perform any error checking. So if you try to do
#' something nonsensical (e.g. have negative values or fractions > 1),
#' the code will likely abort with an error message
#' @examples
#' # To run the simulation with default parameters just call the function:
#' result <- simulate_covid_stochastic()
#' # To choose parameter values other than the standard one, specify them like such:
#' result <- simulate_covid_stochastic(S = 2000,  sigma = 0.1, tmax = 200)
#' # You should then use the simulation result returned from the function, like this:
#' plot(result$ts[ , "time"],result$ts[ , "S"],xlab='Time',ylab='Number Susceptible',type='l')
#' @references See the manual for the pomp package for details on the algorithm.
#' @author Andreas Handel
#' @export


simulate_covid_stochastic <- function(S = 1000, E = 10, beta_d = 5e-4, beta_u = 2.5e-4, beta_e = 1e-4, beta_red_factor = 0.5, t_int1 = 12, sigma = 0.2, gamma_u = 0.1, gamma_d = 0.5, t_int2 = 12, detect_frac_0 = 0.1, detect_frac_1 = 0.2,  t_int3 = 12,  tmax = 100, rngseed = 100)
{

    #######################################################################
    #specify functions for pomp first
    #######################################################################

    #######################################################################
    #1 step for model
    #C-snippet
    #######################################################################
covid_step_C <- pomp::Csnippet("
  double trans[17]; //C indexes at 0, I hate that so I'm making things 1 bigger and start with index 1, i.e. leave trans[0] empty
  double Epresymptom;
  double Idetected;
  double Iundetected;
  double foi; //force of infection
  double gamma; // rate of transition through I compartments
  double detect_frac; //fraction of those that get eventually diagnosed

  Epresymptom = E1+E2+E3+E4+E5+E6;  //all pre-symptomatic
  Idetected = I1+I2+I3+I4;          //all symptomatic that wil be detected
  Iundetected = Iu1+Iu2+Iu3+Iu4;    //all symptomatic/asymptomatic that won't be detected

  //force of infection
  //time dependent transmission, multiplied by different groups
  //each group can have its own transmission rate
  //t_int1 days after simulation start, an intervention reduces transmission rate by some factor
  //t_int1 is new, not in original code. There it was assumed to be the same as t_int2

  if (t<=t_int1)
        foi = beta_d*Idetected + beta_u*Iundetected + beta_e*Epresymptom;
  else
        foi = beta_red_factor*(beta_d*Idetected + beta_u*Iundetected + beta_e*Epresymptom);


  //time-dependent rate of movement through infected and detected classes
  //t_int2 days after simulation start, the time at which individuals are diagnosed and thus the time spent in the I categories before ending up in H decreases
  //t_int2 is caled z in original code

  if (t<t_int2) //if time is less then intervention time, duration spent in I is given by 1/gamma_u, otherwise 1/gamma_d
      gamma = gamma_u;
  else
      gamma = gamma_d;

  //time dependent fraction of those that move into detected category at the end of the E phase
  //t_int3 days after simulation start, the fraction detected (those that move into I instead of Iu after exiting E) increases
  //note that both higher fraction detected and faster rate of detection speed up arrival of individuals in H and subsequently C
  //t_int3 is called w in original code, detect_frac is called q/q0/q1 in the original code

  if (t<t_int3)
    detect_frac = detect_frac_0;
  else
    detect_frac = detect_frac_1;


  // define all transmission rates
  trans[1] = rbinom(S,1-exp(-foi*dt));              //transition from S to E
  trans[2] = rbinom(E1,1-exp(-sigma*dt));           // transition between E compartments 1/2
  trans[3] = rbinom(E2,1-exp(-sigma*dt));           // transition between E compartments
  trans[4] = rbinom(E3,1-exp(-sigma*dt));           // transition between E compartments
  trans[5] = rbinom(E4,1-exp(-sigma*dt));           // transition between E compartments 4/5
  trans[6] = rbinom(E5,1-exp(-sigma*dt));           // transition between E compartments 5/6

  trans[7] = rbinom(E6,(1-exp(-sigma*dt))*detect_frac);           // transition between E6 compartment and I
  trans[8] = rbinom(E6,(1-exp(-sigma*dt))*(1-detect_frac));           // transition between E6 compartment and Iu

  trans[9] = rbinom(I1,1-exp(-gamma*dt));           // transition between I compartments 1/2
  trans[10] = rbinom(I2,1-exp(-gamma*dt));           // transition between I compartments 2/3
  trans[11] = rbinom(I3,1-exp(-gamma*dt));           // transition between I compartments 3/4
  trans[12] = rbinom(I4,1-exp(-gamma*dt));          // transition between I compartments and C

  trans[13] = rbinom(Iu1,1-exp(-gamma_u*dt));           // transition between Iu compartments 1/2
  trans[14] = rbinom(Iu2,1-exp(-gamma_u*dt));           // transition between Iu compartments 2/3
  trans[15] = rbinom(Iu3,1-exp(-gamma_u*dt));           // transition between Iu compartments 3/4
  trans[16] = rbinom(Iu4,1-exp(-gamma_u*dt));           // transition between Iu compartments and Ru

  // define all transmissions for each compartment
  S -= trans[1];

  E1 += trans[1] - trans[2];
  E2 += trans[2] - trans[3];
  E3 += trans[3] - trans[4];
  E4 += trans[4] - trans[5];
  E5 += trans[5] - trans[6];
  E6 += trans[6] - trans[7] - trans[8];

  I1 += trans[7] - trans[9];
  I2 += trans[9] - trans[10];
  I3 += trans[10] - trans[11];
  I4 += trans[11] - trans[12];

  Iu1 += trans[8] - trans[13];
  Iu2 += trans[13] - trans[14];
  Iu3 += trans[14] - trans[15];
  Iu4 += trans[15] - trans[16];

  C += trans[12]; //detected cases, assumed to be isolated and not further contribute to transmission
  Ru += trans[16]; //undetected cases that recover, assumed to not further contribute to transmission

")


    #######################################################################
    #initial conditions for model
    #######################################################################
    covid_init_C <- pomp::Csnippet("
    S = S_0; E1 = E1_0;  E2 = E2_0;  E3 = E3_0;  E4 = E4_0;  E5 = E5_0;  E6 = E6_0;
    I1 = I1_0;  I2 = I2_0;  I3 = I3_0;  I4 = I4_0;
    Iu1 = Iu1_0; Iu2 = Iu2_0; Iu3 = Iu3_0; Iu4 = Iu4_0;  C = C_0; Ru = Ru_0;
    ")

    #######################################################################
    #not currently used since we don't fit
    #added to make pomp stop produce warning messages
    #######################################################################
    rmeas <- pomp::Csnippet("
      cases = C;
    ")

    #variable names
    varnames = c("S", "E1", "E2", "E3", "E4", "E5", "E6", "I1", "I2", "I3", "I4", "Iu1", "Iu2", "Iu3", "Iu4", "C", "Ru")
    #parameter and variable names
    parnames1 = c("beta_d", "beta_u", "beta_e", "beta_red_factor", "t_int1", "t_int2", "t_int3", "gamma_u", "gamma_d", "detect_frac_0","detect_frac_1","sigma")
    #initial conditions of state variables are also parameters
    parnames2 = c("S_0", "E1_0", "E2_0", "E3_0", "E4_0", "E5_0", "E6_0", "I1_0", "I2_0", "I3_0", "I4_0", "Iu1_0", "Iu2_0", "Iu3_0", "Iu4_0", "C_0", "Ru_0")
    parnames = c(parnames1,parnames2)

    #######################################################################
    #create fake data to make pomp happy
    #######################################################################
    fake_data = data.frame(time = 0:tmax, cases = rep(0,length(0:tmax)))

    #######################################################################
    #create pomp object
    #######################################################################

    covid_model_C <- pomp::pomp(data = fake_data,
                          times="time",t0=0,
                          rprocess=pomp::euler(covid_step_C,delta.t=0.05),
                          rmeasure = rmeas,
                          rinit=covid_init_C,
                          paramnames = parnames,
                          statenames = varnames
    )


    set.seed(rngseed) # to allow reproducibility

    #initial conditions for states
    inivals = c(S_0 = S, E1_0 = E, E2_0 = 0, E3_0 = 0, E4_0 = 0, E5_0 =0, E6_0 = 0, I1_0 = 0, I2_0 = 0, I3_0 = 0, I4_0 = 0, Iu1_0 = 0, Iu2_0= 0, Iu3_0= 0, Iu4_0= 0, C_0  = 0, Ru_0 = 0 )

    #values for parameters.
    # gamma and sigma are scaled based on number of dummy compartments
    parvals = c(beta_d = beta_d, beta_u = beta_u, beta_e = beta_e, beta_red_factor = beta_red_factor, t_int1 = t_int1, t_int2 = t_int2, t_int3 = t_int3, gamma_u = 4*gamma_u, gamma_d = 4*gamma_d, detect_frac_0 = detect_frac_0, detect_frac_1 = detect_frac_1, sigma = 6*sigma)

    #run simulation a number of times
    simp <- pomp::simulate(covid_model_C, params=c(parvals,inivals), nsim=1, format="data.frame", include.data=FALSE)
    #sum over dummy compartments, only return totals for each physiological category
    simres = data.frame(time = simp$time, S = simp$S, E = rowSums(simp[,4:9]), I = rowSums(simp[,10:13]), Iu = rowSums(simp[,14:17]), C = simp$C, Ru = simp$Ru )

    result <- list()
    result$ts <- as.data.frame(simres)

    return(result)
}
