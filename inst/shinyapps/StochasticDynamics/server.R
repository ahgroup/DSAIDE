#This is the Shiny server file for the Stochastic Dynamics App

#the main function with all the functionality
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

    # Update the progress bar to show how the process is going
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
    P0 = isolate(input$P0);
    tmax = isolate(input$tmax);

    bP = isolate(input$bP);
    bI = isolate(input$bI);

    gP = isolate(input$gP);
    gI = isolate(input$gI);

    w = isolate(input$w);

    lambda = isolate(input$lambda)
    n = isolate(input$n);
    sigma = isolate(input$sigma)


    # Call the adaptivetau simulator with the given parameters
    result <- simulate_stochastic(PopSize = PopSize, P0 = P0, tmax = tmax, bP = bP,  bI = bI, gP = gP ,  gI = gI, w = w, lambda = lambda, n = n, sigma = sigma)

    return(result)
  })

  # Here, we use the "res" variable to plot the chart that we need
  # the resulting chart will be shown in the "plot" placeholder of the UI
  output$plot <- renderPlot({
    input$submitBtn

    tmax = isolate(input$tmax)

    plot(res()[,1],res()[,"S"],type="l",xlab="time (months)",ylab="",col="green",lwd=2,log="",xlim=c(0,tmax),ylim=c(1,max(res()[,2])),main="Time Series")
    lines(res()[,1],res()[,"P"],type="l",col="orange",lwd=2,lty=1)
    lines(res()[,1],res()[,"I"],type="l",col="red",lwd=2,lty=3)
    lines(res()[,1],res()[,"R"],type="l",col="blue",lwd=2)

    legend("right", c("Susceptible","Pre-Symptomatic","Symptomatic","Recovered"),col = c("green","orange","red",'blue'),lty=c(1,1,2,3),lwd=2)
  }, width = 600, height = 'auto')

  # Use the result "res" returned from the simulator to compute and some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderUI({
    txt <- ""

    PopSize = isolate(input$PopSize)

    Sfinal = round(tail(res()[,"S"],1), 2); Sfracfinal = round(Sfinal / PopSize, 2)
    Pfinal = round(tail(res()[,"P"],1), 2); Pfracfinal = round(Pfinal / PopSize, 2)
    Ifinal = round(tail(res()[,"I"],1), 2); Ifracfinal = round(Ifinal / PopSize, 2)
    Rfinal = round(tail(res()[,"R"],1), 2); Rfracfinal = round(Rfinal / PopSize, 2)
 
    txt1 <- paste(sprintf('Number and Fraction Susceptibles at end of simulation: %.2f, %.2f',Sfinal, Sfracfinal))
    txt2 <- paste(sprintf('Number and Fraction Presymptomatic at end of simulation: %.2f, %.2f',Pfinal, Pfracfinal))
    txt3 <- paste(sprintf('Number and Fraction Symptomatic at end of simulation: %.2f, %.2f',Ifinal, Ifracfinal))
    txt4 <- paste(sprintf('Number and Fraction Recovered at end of simulation: %.2f, %.2f',Rfinal, Rfracfinal))
 
    txt <- paste(txt1, txt2, txt3, txt4, sep = "<br/>")

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
