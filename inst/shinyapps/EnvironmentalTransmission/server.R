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
