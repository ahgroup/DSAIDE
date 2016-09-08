#This is the server file for the Environmental Transmission App

#refresh is the function with all the functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed

refresh <- function(input, output){
  
  # This reactive takes the input data and sends it over to the simulator
  # Then it will get the results back and return it as the "res" variable
  res <- reactive({
    input$submitBtn
    
    # Read all the input values from the UI
    PopSize = isolate(input$PopSize);
    I0 = isolate(input$I0);
    E0 = isolate(input$E0);
    tmax = isolate(input$tmax);
    g  = isolate(input$g);
    bd = isolate(input$bd);
    be = isolate(input$be);
    b  = isolate(input$b);
    n  = isolate(input$n);
    c  = isolate(input$c);
    p  = isolate(input$p);

        
    # Call the ODE solver with the given parameters
    result <- simulate_environmentaltransmission(PopSize = PopSize, I = I0, E = E0, tmax = tmax, bd = bd, be = be, b = b, n = n, g = g, p = p, c = c)

    return (result)
  })
  
  # Here, we use the "odeoutput" variable to plot the chart that we need
  # the resulting chart will be shown in the "plot" placeholder of the UI
  output$plot <- renderPlot(
    {
      input$submitBtn
      
      tmax = isolate(input$tmax)
      PopSize = isolate(input$PopSize);
      
      ymax = max(c(PopSize,res()[,2]))
      
      plot(res()[,1],res()[,2],type="l",xlab="time (months)",ylab="",col="green",lwd=2,log="",xlim=c(0,tmax),ylim=c(1,ymax),main="Time Series")
      lines(res()[,1],res()[,3],type="l",col="red",lwd=2,lty=2)
      lines(res()[,1],res()[,4],type="l",col="gray",lwd=2,lty=3)
      lines(res()[,1],res()[,5],type="l",col="blue",lwd=2,lty=4)
      legend("right", c("Susceptible","Infected","Recovered","Environment"),col = c("green","red","gray","blue"),lwd=2)
    }, width = 600, height = 'auto'
  )
  
  # Use the result "res" returned from the simulator to compute and some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderUI(
    {
      txt <- ""
      
      PopSize = isolate(input$PopSize)
   
      Sfinal = round(tail(res()[,2],1), 2); Sfracfinal = round(Sfinal / PopSize, 2)
      Ifinal = round(tail(res()[,3],1), 2); Ifracfinal = round(Ifinal / PopSize, 2)
      Rfinal = round(tail(res()[,4],1), 2); Rfracfinal = round(Rfinal / PopSize, 2)
      
      txt1 <- paste(sprintf('Number and Fraction Susceptibles at end of simulation: %.2f, %.2f',Sfinal, Sfracfinal))
      txt2 <- paste(sprintf('Number and Fraction Infected at end of simulation: %.2f, %.2f',Ifinal, Ifracfinal))
      txt3 <- paste(sprintf('Number and Fraction Recovered at end of simulation: %.2f, %.2f',Rfinal, Rfracfinal))
      
      txt <- paste(txt1, txt2, txt3, sep = "<br/>")
      
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
