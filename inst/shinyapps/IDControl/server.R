#This is the Shiny server file for the ID Control App

#the main function with all the functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed

refresh <- function(input, output){

    # This reactive takes the input data and sends it over to the simulator
    # Then it will get the results back and return it as the "res" variable
    res <- reactive({
    input$submitBtn
  
    # Read all the input values from the UI
    S0 = isolate(input$S0);
    I0 = isolate(input$I0);
    E0 = isolate(input$E0);
    Sv0 = isolate(input$Sv0);
    Iv0 = isolate(input$Iv0);
    tmax = isolate(input$tmax);

    bP = isolate(input$bP);
    bA = isolate(input$bA);
    bI = isolate(input$bI);
    bE = isolate(input$bE);
    bv = isolate(input$bv);
    bh = isolate(input$bh);
    
    gP = isolate(input$gP);
    gA = isolate(input$gA);
    gI = isolate(input$gI);
    pA = isolate(input$pA);
    pI = isolate(input$pI);
    
    c = isolate(input$c);
    f = isolate(input$f);
    d = isolate(input$d);
    w = isolate(input$w);

    birthh = isolate(input$birthh);
    deathh = isolate(input$deathh);
    birthv = isolate(input$birthv);
    deathv = isolate(input$deathv);
    
    # Call the ODE solver with the given parameters
    result <- simulate_idcontrol(S0 = S0, I0 = I0, E0 = E0, Sv0 = Sv0, Iv0 = Iv0, tmax = tmax, bP = bP, bA = bA, bI = bI, bE = bE, bv = bv, bh = bh, gP = gP , gA = gA, gI = gI, pA = pA, pI = pI, c = c, f = f, d = d, w = w, birthh = birthh, deathh = deathh, birthv = birthv, deathv = deathv)
    
    #browser()
    
    return(result)
  })

  # Here, we use the "res" variable to plot the chart that we need
  # the resulting chart will be shown in the "plot" placeholder of the UI
  output$plot <- renderPlot(
    {
    input$submitBtn

    tmax = isolate(input$tmax)

    par(mfrow=c(2,1))
    
    ymax = max(res()[,2:7])
    plot(res()[,1],res()[,"S"],type="l",xlab="time (months)",ylab="",col="green",lwd=2,log="",xlim=c(0,tmax),ylim=c(1,ymax),main="Time Series")
    lines(res()[,1],res()[,"P"],type="l",col="orange",lwd=2,lty=1)
    lines(res()[,1],res()[,"A"],type="l",col="magenta",lwd=2,lty=2)
    lines(res()[,1],res()[,"I"],type="l",col="red",lwd=2,lty=3)
    lines(res()[,1],res()[,"R"],type="l",col="blue",lwd=2)
    lines(res()[,1],res()[,"D"],type="l",col="gray",lwd=2)
    legend("right", c("Susceptible","Pre-Symptomatic","Asymptomatic","Symptomatic","Recovered","Dead"),col = c("green","orange",'magenta',"red",'blue',"gray"),lty=c(1,1,2,3,1,1),lwd=2)
  
    #2nd panel
    ymax = max(res()[,8:10])
    plot(res()[,1],res()[,"E"],type="l",xlab="time (months)",ylab="",col="green",lwd=2,log="",xlim=c(0,tmax),ylim=c(1,ymax),main="Time Series")
    lines(res()[,1],res()[,"Sv"],type="l",col="blue",lwd=2,lty = 2)
    lines(res()[,1],res()[,"Iv"],type="l",col="red",lwd=2,lty=3)
    legend("right", c("Environment","Susceptible vector","Infected vector"),col = c("green",'blue',"red"),lty=c(1,2,3),lwd=2)
  
    }, height = 'auto' , width = 'auto'
  ) #end renderplot

    # Use the result "res" returned from the simulator to compute and some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderUI({
    txt <- ""

    PopSize = isolate(sum(res()[1,2:7]))

    Sfinal = round(tail(res()[,"S"],1), 2); Sfracfinal = round(Sfinal / PopSize, 2)
    Pfinal = round(tail(res()[,"P"],1), 2); Pfracfinal = round(Pfinal / PopSize, 2)
    Afinal = round(tail(res()[,"A"],1), 2); Afracfinal = round(Afinal / PopSize, 2)
    Ifinal = round(tail(res()[,"I"],1), 2); Ifracfinal = round(Ifinal / PopSize, 2)
    Rfinal = round(tail(res()[,"R"],1), 2); Rfracfinal = round(Rfinal / PopSize, 2)
    Dfinal = round(tail(res()[,"D"],1), 2); Dfracfinal = round(Dfinal / PopSize, 2)

    txt1 <- paste(sprintf('Number and Fraction Susceptibles at end of simulation: %.2f, %.2f',Sfinal, Sfracfinal))
    txt2 <- paste(sprintf('Number and Fraction Presymptomatic at end of simulation: %.2f, %.2f',Pfinal, Pfracfinal))
    txt3 <- paste(sprintf('Number and Fraction Asymptomatic at end of simulation: %.2f, %.2f',Afinal, Afracfinal))
    txt4 <- paste(sprintf('Number and Fraction Symptomatic at end of simulation: %.2f, %.2f',Ifinal, Ifracfinal))
    txt5 <- paste(sprintf('Number and Fraction Recovered at end of simulation: %.2f, %.2f',Rfinal, Rfracfinal))
    txt6 <- paste(sprintf('Number and Fraction Dead at end of simulation: %.2f, %.2f',Dfinal, Dfracfinal))

    txt <- paste(txt1, txt2, txt3, txt4, txt5, txt6, sep = "<br/>")

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
