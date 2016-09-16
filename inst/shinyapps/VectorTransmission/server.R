#This is the server file for the Vector Transmission App

#refresh is the function with all the functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed

refresh <- function(input, output){
  
  # This reactive takes the input data and sends it over to the simulator
  # Then it will get the results back and return it as the "res" variable
  res <- reactive({
    input$submitBtn
    
    # Read all the input values from the UI
    Ph0 = isolate(input$Ph0);
    Ih0 = isolate(input$Ih0);
    Pv0 = isolate(input$Pv0);
    Iv0 = isolate(input$Iv0);
    tmax = isolate(input$tmax);
    
    b1 = isolate(input$b1);
    b2 = isolate(input$b2);
    b  = isolate(input$b);
    n  = isolate(input$n);
    g  = isolate(input$g);
    w  = isolate(input$w);

        
    # Call the ODE solver with the given parameters
    result <- simulate_vectortransmission(Ph0 = Ph0, Ih0 = Ih0, Pv0 = Pv0, Iv0 = Iv0, tmax = tmax, b1 = b1, b2 = b2, b = b, n = n, g = g, w = w)

    return (result)
  })
  
  # Here, we use the "odeoutput" variable to plot the chart that we need
  # the resulting chart will be shown in the "plot" placeholder of the UI
  output$plot <- renderPlot(
    {
      input$submitBtn
      
      tmax = isolate(input$tmax)
      PopSize = isolate(input$PopSize);
      
      ymax = max(c(PopSize,res()[,"Sh"],res()[,"Sv"]))
      
      plot(res()[,1],res()[,2],type="l",xlab="time (months)",ylab="",col="green",lwd=2,log="",xlim=c(0,tmax),ylim=c(1,ymax),main="Time Series")
      lines(res()[,1],res()[,3],type="l",col="red",lwd=2,lty=2)
      lines(res()[,1],res()[,4],type="l",col="gray",lwd=2,lty=3)
      lines(res()[,1],res()[,5],type="l",col="blue",lwd=2,lty=4)
      lines(res()[,1],res()[,6],type="l",col="black",lwd=2,lty=5)
      legend("right", c("Sh","Ih","Rh","Sv","Iv"),col = c("green","red","gray","blue","black"),lwd=2)
    }, width = 600, height = 'auto'
  )
  
  # Use the result "res" returned from the simulator to compute and some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderUI(
    {
      txt <- ""
      
      Phfinal = sum(tail(res()[,2:4],1))
      Pvfinal = sum(tail(res()[,5:6],1))
      
      Shfinal = round(tail(res()[,2],1), 2); Shfracfinal = round(Shfinal / Phfinal, 2)
      Ihfinal = round(tail(res()[,3],1), 2); Ihfracfinal = round(Ihfinal / Phfinal, 2)
      Rhfinal = round(tail(res()[,4],1), 2); Rhfracfinal = round(Rhfinal / Phfinal, 2)

      Svfinal = round(tail(res()[,5],1), 2); Svfracfinal = round(Svfinal / Pvfinal, 2)
      Ivfinal = round(tail(res()[,6],1), 2); Ivfracfinal = round(Ivfinal / Pvfinal, 2)
      
      txt1 <- paste(sprintf('Number and Fraction Susceptibles hosts at end of simulation: %.2f, %.2f',Shfinal, Shfracfinal))
      txt2 <- paste(sprintf('Number and Fraction Infected hosts at end of simulation: %.2f, %.2f',Ihfinal, Ihfracfinal))
      txt3 <- paste(sprintf('Number and Fraction Recovered hosts at end of simulation: %.2f, %.2f',Rhfinal, Rhfracfinal))

      txt4 <- paste(sprintf('Number and Fraction Susceptibles vectors at end of simulation: %.2f, %.2f',Svfinal, Svfracfinal))
      txt5 <- paste(sprintf('Number and Fraction Infected vectors at end of simulation: %.2f, %.2f',Ivfinal, Ivfracfinal))
      
      txt <- paste(txt1, txt2, txt3, txt4, txt5, sep = "<br/>")
      
      HTML(txt)
    })
  
  
  # At last, if we have any warnings or error from the "odeoutput" we can show them here
  # These pieces of texts will be shown in red in the UI ("warn" placeholder will be used)
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

shinyServer(function(input, output, session) {
  
  # Waits for the Exit Button to be pressed to stop the app server
  observeEvent(input$exitBtn, {
    input$exitBtn
    stopApp(returnValue = 0)
  })
  
  # This function is called to refresh the content of the UI
  refresh(input, output)
  
  # Event handler to listen for the webpage and see when it closes.
  # Right after the window is closed, it will stop the app server and the main menu will
  # continue asking for inputs.
  session$onSessionEnded(function(){
    stopApp(returnValue = 0)
  })
  
})
