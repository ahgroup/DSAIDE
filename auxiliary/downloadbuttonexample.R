#load DSAIDE package
library(DSAIDE)
#runs simulation with provided parameter values
simresult = simulate_sir_ode(S = 1000, I = 1, R = 0, b = 0.001, g = 0.5, tstart = 0, tfinal = 100, dt = 0.1)
#save result in a list structure, needed to use generate_plot and generate_text functions
result = vector("list", 1) #create a list to feed to the generate functions
result[[1]]$ts = simresult$ts
# use built-in functions to generate plot(s) and text
plotresult <- generate_plots(result) #contains plot(s)
textresult <- generate_text(result) #contains written information shown below plot