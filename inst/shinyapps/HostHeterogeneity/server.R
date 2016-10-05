#This is the Shiny server file for the Host Heterogeneity App

#the main function with all the functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed

refresh <- function(input, output){

    # This reactive takes the input data and sends it over to the simulator
    # Then it will get the results back and return it as the "res" variable
    res <- reactive({
    input$submitBtn
  
    # Read all the input values from the UI
    S10 = isolate(input$S10);
    I10 = isolate(input$I10);
    S20 = isolate(input$S20);
    I20 = isolate(input$I20);
    tmax = isolate(input$tmax);

    b11 = isolate(input$b11);
    b21 = isolate(input$b21);
    b12 = isolate(input$b12);
    b22 = isolate(input$b22);

    g1 = isolate(input$g1);
    g2 = isolate(input$g2);

    w1 = isolate(input$w1);
    w2 = isolate(input$w2);

    # Call the ODE solver with the given parameters
    result <- simulate_heterogeneity(S10 = S10, I10 = I10, S20 = S20, I20 = I20, tmax = tmax, b11 = b11, b12 = b12, b21 = b21, b22 = b22, g1 = g1 , g2 = g2, w1 = w1, w2 = w2)
    
    #browser()
    
    return(result)
  })

  # Here, we use the "res" variable to plot the chart that we need
  # the resulting chart will be shown in the "plot" placeholder of the UI
  output$plot <- renderPlot(
    {
    input$submitBtn

    tmax = isolate(input$tmax)

   
    ymax = max(res()[,-1])
    plot(res()[,1],res()[,"S1"],type="l",xlab="time (months)",ylab="",col="blue",lwd=2,log="",xlim=c(0,tmax),ylim=c(1,ymax),main="Time Series")
    lines(res()[,1],res()[,"I1"],type="l",col="red",lwd=2,lty=1)
    lines(res()[,1],res()[,"R1"],type="l",col="gray",lwd=2,lty=1)
    lines(res()[,1],res()[,"S2"],type="l",col="blue",lwd=2,lty=2)
    lines(res()[,1],res()[,"I2"],type="l",col="red",lwd=2,lty=2)
    lines(res()[,1],res()[,"R2"],type="l",col="gray",lwd=2,lty=2)
    legend("right", c("Sus type 1","Inf type 1","Rec type 1","Sus type 2","Inf type 2","Rec type 2"),col = c("blue","red","gray"),lty=c(1,1,1,2,2,2),lwd=2)
  

    }, height = 'auto' , width = 'auto'
  ) #end renderplot

    # Use the result "res" returned from the simulator to compute and some text results
  # the text should be formatted as HTML and placed in the "text" placeholder of the UI
  output$text <- renderUI({
    txt <- ""

    PopSize1 = isolate(sum(res()[1,2:4]))
    PopSize2 = isolate(sum(res()[1,5:7]))
    
    S1final = round(tail(res()[,"S1"],1), 2); S1fracfinal = round(S1final / PopSize1, 2)
    I1final = round(tail(res()[,"I1"],1), 2); I1fracfinal = round(I1final / PopSize1, 2)
    R1final = round(tail(res()[,"R1"],1), 2); R1fracfinal = round(R1final / PopSize1, 2)

    S2final = round(tail(res()[,"S2"],1), 2); S2fracfinal = round(S2final / PopSize2, 2)
    I2final = round(tail(res()[,"I2"],1), 2); I2fracfinal = round(I2final / PopSize2, 2)
    R2final = round(tail(res()[,"R2"],1), 2); R2fracfinal = round(R2final / PopSize2, 2)
    
    txt1 <- paste(sprintf('Number and Fraction type 1 Susceptibles at end of simulation: %.2f, %.2f',S1final, S1fracfinal))
    txt2 <- paste(sprintf('Number and Fraction type 1 Infected at end of simulation: %.2f, %.2f',I1final, I1fracfinal))
    txt3 <- paste(sprintf('Number and Fraction type 1 Recovered at end of simulation: %.2f, %.2f',R1final, R1fracfinal))

    txt4 <- paste(sprintf('Number and Fraction type 1 Susceptibles at end of simulation: %.2f, %.2f',S2final, S2fracfinal))
    txt5 <- paste(sprintf('Number and Fraction type 1 Infected at end of simulation: %.2f, %.2f',I2final, I2fracfinal))
    txt6 <- paste(sprintf('Number and Fraction type 1 Recovered at end of simulation: %.2f, %.2f',R2final, R2fracfinal))
    
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
