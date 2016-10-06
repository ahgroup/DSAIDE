#This is the Shiny server file for the ID Patterns App

#the main function with all the functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed

refresh <- function(input, output){

    # This reactive takes the input data and sends it over to the simulator
    # Then it will get the results back and return it as the "res" variable
    res <- reactive({
    input$submitBtn

    
    # Read all the input values from the UI
    PopSize = isolate(input$PopSize);
    P0 = isolate(input$P0);
    tmax = isolate(input$tmax);

    bP = isolate(input$bP);
    bA = isolate(input$bA);
    bI = isolate(input$bI);

    gP = isolate(input$gP);
    gA = isolate(input$gA);
    gI = isolate(input$gI);

    f = isolate(input$f);
    d = isolate(input$d);
    w = isolate(input$w);

    lambda = isolate(input$lambda)
    n = isolate(input$n);
    sigma = isolate(input$sigma)


    # Call the ODE solver with the given parameters
    result <- simulate_idpatterns(PopSize = PopSize, P0 = P0, tmax = tmax, bP = bP, bA = bA, bI = bI, gP = gP , gA = gA, gI = gI, f = f, d = d, w = w, lambda = lambda, n = n, sigma = sigma)

    return(result)
    })
    
    #function that takes result saved in res and produces output
    #output (plots, text, warnings) is stored in and modifies the global variable output
    produce_simoutput(input,output,res)
    
} #ends inner shiny server function that runs the simulation and returns output

    
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
