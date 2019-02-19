##################################################################################
#fitting mortality data from the 1918 influenza pandemic to an SIR model to estimate R0
##written by Andreas Handel, ahandel@uga.edu, last change 12/21/11
##################################################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
require(deSolve)  #loads ODE solver package
                
#################################
#experimental data values from Mills et al. 2004 Nature 
#one could load the data from a separate file. for simplicity, we include it as part of the program
################################
timedata=1:15 #week of outbreak
deaths=c(6, 83, 592,1904,3896,4830,4039,2008,832,440,202,205,218,262,393); #weekly deaths per 100,000
#--> convert the weekly deaths into cumulative deaths and store them in the variable cumdeaths
cumdeaths=cumsum(deaths) #cummulative deaths

###################################################################
#function specifying the ode model. This is called by the ode solver inside the fit function
###################################################################
odeequations=function(t,y,parameters) 
{ 
  	Sus=y[1]; Infc=y[2]; Dead=y[3];  #uninfected, infected, dead - we can ignore the recovered
	  b=parameters[1]; f=parameters[2]; 
                                  
	  #these are the 3 differential equations
		dSus=-b*Sus*Infc;
		dInfc=b*Sus*Infc-gam*Infc;
		dDead=f*gam*Infc;
 
		return(list(c(dSus,dInfc,dDead))); 

} #end function specifying the ODEs


###################################################################
#function that fits the ODE model to data 
###################################################################
fitfunction <- function(parameters,timedata,deathdata)
{

	  Infc0=parameters[3]; #initial number of infected, is being fitted 
    Y0=c(Sus0, Infc0, Dead0);  #combine initial conditions into a vector 
	      	
    #call ode-solver lsoda to integrate ODEs 
    odeoutput=lsoda(Y0,timevec,odeequations,parameters,atol=1e-10);
    deathmodel=odeoutput[seq(11,151,10),4]; #extract values for dead at time points corresponding to data values, i.e. every week  
  
    #plot data and model to watch fitting in real time. plot is done on a log scale
    if (ploton==1)
    {
      plot(timedata,log10(deathdata),type="p",xlim=c(0,15),ylim=c(-1,7),col="red",xlab="time (weeks)",ylab="people");
      lines(odeoutput[,1],log10(pmax(1e-10,odeoutput[,4])),col="red")
      lines(odeoutput[,1],log10(pmax(1e-10,odeoutput[,2])),col="blue");
      lines(odeoutput[,1],log10(pmax(1e-10,odeoutput[,3])),col="green");
      points(odeoutput[seq(11,151,10),1],log10(deathmodel),type="p",col="red",pch=3)  
      legend('bottomright',c('dead','susceptible','infected'),col=c('red','blue','green'),lwd=2)       
    }
    #return the objective function, the sum of squares, which is being minimized by optim 
    SSR=sum((log10(deathmodel)-log10(deathdata))^2) #fit is done on a log scale
    #--> after doing the fit on a log scale, also fit the data on a linear scale. 
    #to do so, comment out the command above and write an equivalent one that computes SSR for data and model in linear space
    #SSR=sum((deathmodel-deathdata)^2) #linear
    
    return(SSR) 
} #end function that fits the ODE model to the data

############################################################
#the main part, which calls the fit function 
############################################################

maxsteps=1000; #maximum number of iterations for the optimization routine 

#--> Find a value for Sus0, i.e. the number of susceptibles living in NYC in 1918
Sus0=5.6e6; #1918 NYC population - we assume everyone is susceptible
Dead0=0; #initial number of deaths
timevec=seq(0, 15,0.1); #vector of times (in units of weeks) for which integration is evaluated
#--> The data are deaths per 100,000. This needs to be converted to total deaths. Write a line that does this conversion, i.e. convert the data in cumdeaths into total deaths
deathdata=cumdeaths/100000*Sus0; #rescale death data for full population

#starting guesses for the parameters that will be fit. units are in weeks
b0=1e-6;  
Infc00=10; 
f0=0.2;   

gam=1; #we fix duration of infection, 1/gam to one week
ploton=1;  #switches on/off plotting during fitting 
#uses the optimization routine optim to perform the fit
x0=c(b=b0,f=f0,Infc0=Infc00);
fitresult <- optim(par=x0, fn=fitfunction, gr = NULL, timedata=timedata, deathdata=deathdata, method = "Nelder-Mead",control=list(trace=0,maxit=maxsteps,parscale=x0))
finalparams=fitresult$par

############################################################
#output result
############################################################

#compute model solution for initial parameter values
Y0=c(Sus0, Infc00, Dead0);  #combine initial conditions into a vector 
odeoutputini=lsoda(Y0,timevec,odeequations,x0);
deathmodelini=odeoutputini[seq(11,151,10),4]; 

#compute model solution for final parameter values
b=finalparams[1]; f=finalparams[2]; Infc0=finalparams[3]; 
Y0=c(Sus0, Infc0, Dead0);  #combine initial conditions into a vector 
odeoutput=lsoda(Y0,timevec,odeequations,c(b,f,Infc0));
deathmodel=odeoutput[seq(11,151,10),4]; 

ssrini=sum((log10(deathmodelini)-log10(deathdata))^2); ssrfinal=sum((log10(deathmodel)-log10(deathdata))^2);
#--> when you fit on the linear scale, also report final SSR in linear space
#ssrini=sum((deathmodelini-deathdata)^2); ssrfinal=sum((deathmodel-deathdata)^2);

#--> compute R0 
R0=b/gam*Sus0;

print(sprintf('Initial guess - Mortality fraction: %f, Initial Infected: %f, R0: %f, SSR=%f',f0,Infc00,b0/gam*Sus0,ssrini));
#--> print best fit values for f, Initial number of infected, and R0. Also print final SSR
print(sprintf('Best fit - Mortality fraction: %f, Initial Infected: %f, R0: %f, SSR=%f',f,Infc0,R0,ssrfinal));

#plot data and model solution for initial and final parameter values
graphics.off(); #close graphics window     
plot(timedata,log10(deathdata),type="p",xlim=c(0,15),ylim=c(-1,7),col="red",ylab="Deaths (log scale)",xlab="time (weeks)");
lines(odeoutputini[,1],log10(pmax(1e-10,odeoutputini[,4])),col="blue");
lines(odeoutput[,1],log10(pmax(1e-10,odeoutput[,4])),col="black")
legend('bottomright',c('data','model initial','model final'),col=c('red','blue','black'),pch=c(19,-1,-1),lty=c(-1,1,1),lwd=2)       
 
###################################################################
#end main program
###################################################################