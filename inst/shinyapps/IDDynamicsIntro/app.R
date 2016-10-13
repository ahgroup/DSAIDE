#This is the Shiny file for the ID Dynamics Introduction App

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
    b = isolate(input$b);
    g = isolate(input$g);
    tmax = isolate(input$tmax);

    # Call the ODE solver with the given parameters
    result <- simulate_introduction(S0 = S0, I0 = I0, g = g, b = b, tmax = tmax)
    
    return(list(result))
  })
  
  #function that takes result saved in res and produces output
  #output (plots, text, warnings) is stored in and modifies the global variable output
  produce_simoutput(input,output,res)
  
} #ends inner shiny server function that runs the simulation and returns output


server <- function(input, output, session) {
  
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
  
}


#This is the UI for the ID Dynamics Introduction App
ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  h1('ID Dynamics Introduction App', align = "center", style = "background-color:#123c66; color:#fff"),
  
  #start section to add buttons
  fluidRow(
    column(6,
           actionButton("submitBtn", "Run Simulation", class="submitbutton")  
    ),
    column(6,
           actionButton("exitBtn", "Exit App", class="exitbutton")
    ),
    align = "center"
  ), #end section to add buttons
  
  tags$hr(),
  
  ################################
  #Split screen with input on left, output on right
  fluidRow(
    #all the inputs in here
    column(6,
           #################################
           # Inputs section
           h2('Simulation Settings'),
           fluidRow(
             column(4,
                    sliderInput("S0", "Initial number of susceptible hosts", min = 500, max = 5000, value = 1000, step = 500)
             ),
             column(4,
                    sliderInput("I0", "Initial number of infected hosts", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    sliderInput("tmax", "Maximum simulation time", min = 10, max = 1000, value = 300, step = 10)
             ),
             align = "center"
           ), #close fluidRow structure for input
           
           fluidRow(
             column(6,
                    sliderInput("b", "Rate of transmission", min = 0, max = 0.01, value = 0, step = 0.0001, sep ='')
             ),
             column(6,
                    sliderInput("g", "Rate at which a host leaves the infectious compartment", min = 0, max = 2, value = 0.5, step = 0.1)
             ),
             align = "center"
           ) #close fluidRow structure for input
           
           
    ), #end sidebar column for inputs
    
    #all the outcomes here
    column(6,
           
           #################################
           #Start with results on top
           h2('Simulation Results'),
           plotOutput(outputId = "plot", height = "500px"),
           # PLaceholder for results of type text
           htmlOutput(outputId = "text"),
           #Placeholder for any possible warning or error messages (this will be shown in red)
           htmlOutput(outputId = "warn"),
           
           tags$head(tags$style("#warn{color: red;
                                font-style: italic;
                                }")),
           tags$hr()
           
           ) #end main panel column with outcomes
  ), #end layout with side and main panel
  
  #################################
  #Instructions section at bottom as tabs
  h2('Instructions'),
  #use external function to generate all tabs with instruction content
  do.call(tabsetPanel,generate_instruction_tabs()),
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
  
  ) #end fluidpage, i.e. the UI part of the app

shinyApp(ui = ui, server = server)