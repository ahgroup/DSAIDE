##################################################################################
#fitting case data from norovirus outbreaks to several simple models
##written by Andreas Handel, ahandel@uga.edu, last change 11/12/11
##################################################################################
rm(list=ls()) #this clears the workspace to make sure no leftover variables are floating around. Not strictly needed
graphics.off(); #close all graphics windows
require(deSolve)  #loads ODE solver package
require(nloptr) #for fitting

eps=1e-12; #some low value, needed for some cut-offs
                
#################################
#experimental data values 
################################
#data from Kuo 2009 Wien Klin Woch: "A non-foodborne norovirus outbreak among school children during a skiing holiday, Austria, 2007"
#outbreak was in december, times are dates in december at which clinical onset occured
#school 1, N=44
#schools 2-4, N=240 (no individual N per school available)
#1st case occured on december 8
#data is new cases starting at the given date
################################
Ntot=44+240;
times=c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
counts1=c(1,0, 1,  8, 10,  7,  1,  0,  0,  0) #school 1 cases
counts2=c(0,0, 1,  36, 29,  6,  1,  2,  0,  1) #school 2 cases
counts3=c(0,0, 1,  9, 20,  4,  0,  0,  1,  2) #school 3 cases
counts4=c(0,0, 1,  13, 17,  1,  2,  1,  0,  0) #school 4 cases

timedata=times;
ydata.new=counts1+counts2+counts3+counts4; #for our purpose we combine cases from all 4 schools and don't discriminate between schools
ydata.all=cumsum(ydata.new); #we can either fit incidence or cumulative incidence. The latter sometimes leads to more robust results, we'll try both here
tmax=timedata[length(timedata)];


###################################################################
#function specifying the ode model. This is called by the ode solver inside the fit function
###################################################################
odeequations=function(t,y,p) 
{ 
  	S=y[1]; In=y[2]; R=y[3]; Itot=y[4];  

    if (scenario==1)  #we are fitting 3 different models, depending on the scenario variable, different ones are chosen
    {
      b=p[1]; d=p[2];  lambda=0; #no external source 
    }
    else if (scenario==2) 
    {
      b=p[1]; d=p[2]; lambda=p[3]; #with extra constant infection term
    }
   else if (scenario==3) 
    {
      b=p[1]; d=p[2]; lambda=0; 
      t1=p[4]; t2=p[5];  
      if (t1 < t && t < t2) {lambda=p[3];} #with step-function environmental source 
    }
	                                
	  #these are the differential equations
		dS=-b*S*In-lambda*S;
    dI=lambda*S + S*b*In - d*In;
		dR=d*In;
    dItot=lambda*S + S*b*In;
 
		return(list(c(dS,dI,dR,dItot))); 

} #end function specifying the ODEs


###################################################################
#function that fits the ODE model to data 
###################################################################
fitfunction <- function(parameters)
{

    
    odeoutput=lsoda(Y0,timevec,odeequations,parameters,atol=1e-10);     #call ode-solver lsoda to integrate ODEs 


    modelfit.all=odeoutput[match(timedata,odeoutput[,1]),5]; #cumulative cases
    modelfit.new=c(I0,diff(modelfit.all)); #new cases
        
    if (ploton==1)  #just for plotting purposes, if we want to see "fitting in action"
    {
      par(mfrow=c(1,2))
      cxl=1.75; cxa=1.75;
      plot(timedata,ydata.all,type="p",xlim=c(timedata[1],tmax+2),ylim=c(0,200),col="red",xlab="time (days)",ylab="hosts",lwd=2,cex.axis=cxa,cex.lab=cxl,pch=19);
      lines(odeoutput[,1],odeoutput[,5],col="blue",lwd=2) #this plots cumulative cases
      legend('topright',c('data','model'),col=c('red','blue'),lwd=2,lty=c(-1,1),pch=c(19,-1),merge=TRUE)       
      
      plot(timedata,ydata.new,type="p",xlim=c(timedata[1],tmax+2),ylim=c(0,100),col="red",xlab="time (days)",ylab="hosts",lwd=2,cex.axis=cxa,cex.lab=cxl,pch=19);
      points(timedata,modelfit.new,col="blue",lwd=2,pch=20); #this plots new cases
      lines(odeoutput[,1],odeoutput[,3],col="green",lwd=2,lty=1); #this plots new cases
      legend('topright',c('data','model','I'),col=c('red','blue','green'),lwd=2,lty=c(-1,-1,1),pch=c(19,20,-1),merge=TRUE)       
    }
    
    NLL.all=-sum(dpois(ydata.all,lambda=modelfit.all,log = TRUE));   #this computes the negative log likelihood for either a fit of model to cumulative incidence or just incidence
    NLL.new=-sum(dpois(ydata.new,lambda=modelfit.new,log = TRUE));
    
    #depending on which of the NLL we return, we fit either cumulative cases or just incidence
    #return(NLL.all) #return as the objective function the likelihood for fitting the cumulative cases
    return(NLL.new) #return as the objective function the likelihood for fitting the new cases
     
} #end function that fits the ODE model to the data


############################################################
#the main part, which calls the fit function 
############################################################
                
S0=Ntot-1; #total pop  
I0=ydata.new[1];
R0=0; #number of initially recovered (not R0=transmissibility measure)
Y0=c(S0, I0, R0, I0);  #combine initial conditions into a vector 
timevec=seq(timedata[1], tmax,0.02); #vector of times (in units of days) for which integration is evaluated

#bounds for the parameters that will be fit. units are in days
blow=1e-12;    bhigh=1e6;
dlow=1/60; dhigh=10;      #bounds are 0.1 days and 10 days
lamlow=1e-8; lamhigh=1e3;
t1low=1e-3; t1high=tmax;
t2low=1e-3; t2high=tmax;

#some general settings
ploton=0; stps=1000; mtime=60*60; constr=1; 

tstart=proc.time(); #capture current time to measure speed

#by commenting/uncommenting each "Scenario" block below, one can fit the different models

#scenario with no external infection term 
scenario=1;
p0=c(1e-2,2); #initial guess for parameters
lb=c(blow,dlow); ub=c(bhigh,dhigh);

#scenario with constant external infection term 
#scenario=2;
#p0=c(1e-2,2,0.01) #initial guess for parameters
#lb=c(blow,dlow,lamlow); ub=c(bhigh,dhigh,lamhigh);

#scenario with step-function external infection term
#scenario=3;
#p0=c(1e-4,2,0.1,10,12); #initial guess for parameters
#lb=c(blow,dlow,lamlow,t1low,t2low); ub=c(bhigh,dhigh,lamhigh,t1high,t2high);


#this line does the actual fitting
fres <- nloptr(x0=p0,eval_f=fitfunction,lb=lb,ub=ub,opts=list("algorithm"="NLOPT_LN_SBPLX",xtol_rel=1e-12,maxeval=stps,maxtime=mtime,print_level=0));
print(sprintf('Finished Scenario %d using solver %s',scenario, fres$call[[6]][[2]]));

#extract best fit parameter values and final NLL from the result returned by the optimizer
fpar=(fres$solution); NLL.final=fres$objective
                                  
tend=proc.time(); #capture current time
tdiff=tend-tstart;
print(sprintf('Optimization took %f seconds',tdiff[3])); 

dp=length(ydata.all); #number of data points
xm=length(p0); #number of parameters 

AIC=2*xm+2*NLL.final; AICc=AIC+2*xm*(xm+1)/(dp-xm-1); #used if all parameters, including nuisance ones are fit. 
if ((dp-xm-1)<1) {AICc=NA} #test if we have too many parameters for AICc to makes sense

#print best fit parameter values
cat(sprintf('%e,',fpar),sprintf('); NLL.final=%e; AICc=%e;\n',NLL.final,AICc));

#play alarm to signal end of run
alarm(); Sys.sleep(0.5); alarm(); Sys.sleep(0.5); alarm();

###################################################################
#end main program
###################################################################