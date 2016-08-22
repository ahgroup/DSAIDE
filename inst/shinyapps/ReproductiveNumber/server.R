#This is the server file for the Reproductive Number App

#refresh is the function with all the functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed

refresh <- function(input, output){

  # This reactive takes the input data and sends it over to the simulator
  # Then it will get the results back and return it as the "res" variable
  res <- reactive({
    input$submitBtn

    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Starting Simulation: ", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())

    # Update the proggress bar to show how the process is going
    updateProgress_ <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }else{
        value <- progress$getMax() * value
      }
      progress$set(value = value, detail = detail)
    }

    # Read all the input values from the UI
    PopSize = isolate(input$PopSize);
    I0 = isolate(input$I0);
    f = isolate(input$f);
    tmax = isolate(input$tmax);
    gamma = isolate(input$gamma);
    beta = isolate(input$beta);
    lambda = isolate(input$lambda);
    n = isolate(input$n);
    e = isolate(input$e);
    
    # Call the ODE solver with the given parameters
    result <- simulate_reproductivenumber(PopSize = PopSize, I0 = I0, f = f, e=e, tmax = tmax, gamma = gamma, beta = beta, lambda = lambda, n = n)
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
    lines(res()[,1],res()[,3],type="l",col="red",lwd=2)
    lines(res()[,1],res()[,4],type="l",col="gray",lwd=2)
    lines(res()[,1],res()[,2]+res()[,3]+res()[,4],type="l",col="blue",lwd=2)
    legend("right", c("Susceptible","Infected","Recovered","Total"),col = c("green","red","gray","blue"),lwd=2)
  }, width = 600, height = 'auto'
  )

  # Use the result "res" returned from the simulator to compute and some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderUI(
  {
    txt <- ""

    PopSize = isolate(input$PopSize)
    gamma = isolate(input$gamma)
    beta = isolate(input$beta)
    I0 = isolate(input$I0);
    f = isolate(input$f);
    e = isolate(input$e);

    
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
