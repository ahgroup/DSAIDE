#' @title A helper function that takes result from the simulators and produces plots and text output
#'
#' @description This function generates plots and text to be displayed in the Shiny UI. 
#' This is a helper function. This version can process multiple simulation runs, supplied as a list
#' @return output a list with plot, text and warn elements for display in a shiny UI
#' @details This function is called by the shiny server to produce output returned to the UI
#' @author Andreas Handel
#' @keywords internal
#' @export


produce_multisimoutput <- function(input,output,allres)
{
  
# Here, we use the result returned from the ode solver to produce the plot
  # the resulting plot is saved in the "plot" placeholder of the output variable
  output$plot <- renderPlot({
    input$submitBtn

    tmax = isolate(input$tmax)
    nreps = isolate(input$nreps)
    
    #process first simulation to build plot
    res = allres()[[1]]      
    ymax = max(res[,-1])
    tvec = res[,1]
    mycols=c("blue",'orange','red','green','black','magenta','cyan')
    
    plot(tvec,res[,2],type="l",xlab="time (days)",ylab="",col=mycols[1],lwd=1,log="",xlim=c(0,tmax),ylim=c(0,ymax),main="Time Series")
    for (nn in 3:ncol(res))
    {
      lines(tvec,res[,nn],type="l",col=mycols[nn-1],lwd=1,lty=1)
    }
    legend("right", colnames(res)[-1],col = mycols,lty=c(1),lwd=2)
    
        
    #loop over each additional simulation
    #results are added to plot
    for (n1 in 2:nreps)
    {
        res = allres()[[n1]]      
        tvec = res[,1]
        lines(tvec,res[,2],type="l",col=mycols[1],lwd=1,lty=1)
        for (nn in 3:ncol(res))
        {
          lines(tvec,res[,nn],type="l",col=mycols[nn-1],lwd=1,lty=1)
        }
    } #done additing lines from additional runs

    }, width = 'auto', height = 'auto'
  ) #end plot
  
  # Use the result "res" returned from the simulator to compute and some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderUI({
    nreps = isolate(input$nreps)
    ncols = ncol(allres()[[1]])-1 #number of variables, minus time
    resfinal = rep(0,ncols) 
    resfracfinal = rep(0,ncols) 
    varnames = colnames(allres()[[1]])[-1]
    for (n1 in 1:nreps) #add all final values
    {
      currfinal = tail(allres()[[n1]],1)[-1] #final number for each variable, excluding time
      resfinal = resfinal + currfinal #total numbers
      resfracfinal = resfracfinal + currfinal / sum(currfinal) #add up fractions
    }  
    resfinal = resfinal/nreps #mean for each variable, take out time
    resfracfinal = resfracfinal/nreps #mean for each variable, take out time
    
    txt <- ""
    for (nn in 1:ncols)
    {
      numfinal = round(resfinal[nn], 2);
      fracfinal = round(resfracfinal[nn], 2)
      newtxt <- paste('Average (Mean) Number and Fraction of ',varnames[nn],' at end of simulation: ',numfinal,' and ',fracfinal,sep='')
      txt <- paste(txt, newtxt, sep = "<br/>")
    }
    HTML(txt)
  }) #end text output
    
  # At last, if we have any warnings or error from the simulator we can show them here
  # That text will be shown in red in the UI ("warn" placeholder will be used)
  output$warn <- renderUI({
    txt <- ""
    if(length(data()$warns) == 0){
      
    }else{
      txt <- paste(txt, "Warnings:", sep = "<br/>")
      for (i in 1:length(data()$warns)){
        txt <- paste(txt, data()$warns[[i]], sep = "<br/>")
      }
    }
    HTML(txt)
  })
}
