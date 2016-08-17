getPage <-function(filepath) {
  return(includeHTML(sprintf("www/%s", filepath)))
}

refresh <- function(input, output){
  odeoutput <- reactive({
    input$submitBtn

    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Starting Simulation: ", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())

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
    R0 = isolate(input$R0);
    tmax = isolate(input$tmax);
    gamma = isolate(input$gamma);
    beta.d = as.numeric(isolate(input$betaD));
    beta.f = isolate(input$betaF);
    mu = isolate(input$mu);
    w = isolate(input$w);
    k = as.numeric(isolate(input$k));
    scenario = isolate(input$scenario);

    # Call the ODE solver with the given parameters
    result <- simulate_transmissionmodes(PopSize = PopSize, I0 = I0, R0 = R0, tmax = tmax, gamma = gamma, beta.d = beta.d, beta.f = beta.f,
                               mu = mu, w = w, k = k, scenario = scenario)
    return (result)
  })

  output$plot <- renderPlot({
    input$submitBtn

    tmax = isolate(input$tmax)
    PopSize = isolate(input$PopSize)
    I0 = isolate(input$I0)

    plot(odeoutput()[,1], odeoutput()[,2], type = "l", xlab = "time (years)", ylab = "", col = "green", lwd = 2,
         log = "", xlim = c(0, tmax), ylim = c(1, PopSize - I0), main = "Outbreak Time Series")
    lines(odeoutput()[,1], odeoutput()[,3], type = "l", col = "red", lwd = 2)
    lines(odeoutput()[,1], odeoutput()[,4], type = "l", col = "gray", lwd = 2)
    lines(odeoutput()[,1], odeoutput()[,2] + odeoutput()[,3] + odeoutput()[,4], type = "l", col = "blue", lwd = 2)
    legend("right", c("Susceptible", "Infected", "Recovered", "Total"), col = c("green", "red", "gray", "blue"), lwd = 2)
  })

  output$text <- renderUI({
    txt <- ""

    txt1 <- paste(sprintf('Number of susceptibles/infected/recovered at end of simulation: %f/%f/%f',
                          tail(odeoutput()[,2], 1),
                          tail(odeoutput()[,3], 1),
                          tail(odeoutput()[,4], 1)))
    txt2 <- paste(sprintf('\n\nPercent of susceptibles/infected/recovered at end of simulation: %f/%f/%f',
                          tail(odeoutput()[,2], 1) * 100 / sum(tail(odeoutput()[,2:4], 1)),
                          tail(odeoutput()[,3], 1) * 100 / sum(tail(odeoutput()[,2:4], 1)),
                          tail(odeoutput()[,4], 1) * 100 / sum(tail(odeoutput()[,2:4], 1))))

    txt <- paste(txt1, txt2, sep = "<br/>")

    HTML(txt)
  })

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

  observeEvent(input$exitBtn, {
    input$exitBtn
    stopApp(returnValue = 0)
  })

  refresh(input, output)

  session$onSessionEnded(function(){
    stopApp(returnValue = 0)
  })

})
