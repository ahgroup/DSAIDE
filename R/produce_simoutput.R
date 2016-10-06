#' @title A helper function that takes result from the simulators and produces plots and text output
#'
#' @description This function generates plots and text to be displayed in the Shiny UI. This is a helper function.
#' @return output a list with plot, text and warn elements for display in a shiny UI
#' @details This function is called by the shiny server to produce output returned to the UI
#' @author Andreas Handel
#' @keywords internal
#' @export


produce_simoutput <- function(input,output,res)
{
  
# Here, we use the result returned from the ode solver to produce the plot
  # the resulting plot is saved in the "plot" placeholder of the output variable
  output$plot <- renderPlot({
    input$submitBtn

    tmax = isolate(input$tmax)
    
    ymax = max(res()[,-1])
    tvec = res()[,1]
    mycols=c("blue",'orange','red','green','black','magenta','cyan')
    
    plot(tvec,res()[,2],type="l",xlab="time (days)",ylab="",col=mycols[1],lwd=2,log="",xlim=c(0,tmax),ylim=c(0,ymax),main="Time Series")
    for (nn in 3:ncol(res()))
    {
      lines(tvec,res()[,nn],type="l",col=mycols[nn-1],lwd=2,lty=1)
    }
    
    legend("right", colnames(res())[-1],col = mycols,lty=c(1),lwd=2)
  }, width = 'auto', height = 'auto'
  ) #end plot
  
  # Use the result "res" returned from the simulator to compute and some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderUI({
    txt <- ""
    PopSize = sum(tail(res()[,-1],1)) #total pop size at end of simulation
    for (nn in 2:ncol(res()))
    {
      numfinal = round(tail(res()[,nn],1), 2);
      fracfinal = round(numfinal / PopSize, 2)
      newtxt <- paste('Number and Fraction of ',colnames(res())[nn],' at end of simulation: ',numfinal,' and ',fracfinal,sep='')
      txt <- paste(txt, newtxt, sep = "<br/>")
    }

    HTML(txt)
  })
    
  # At last, if we have any warnings or error from the "res" we can show them here
  # These peices of texts will be shown in red in the UI ("warn" placeholder will be used)
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
