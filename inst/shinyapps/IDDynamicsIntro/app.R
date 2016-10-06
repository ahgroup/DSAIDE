#This is the Shiny file for the ID Dynamics Introduction App

#the main function with all the functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed

refresh <- function(input, output){
  
  # This reactive takes the input data and sends it over to the simulator
  # Then it will get the results back and return it as the "res" variable
  res <- reactive({
    input$submitBtn
    
    
    # Read all the input values from the UI
    PopSize = isolate(input$PopSize);
    I0 = isolate(input$I0);
    beta = isolate(input$beta);
    gamma = isolate(input$gamma);
    tmax = isolate(input$tmax);

    # Call the ODE solver with the given parameters
    result <- simulate_introduction(PopSize = PopSize, I0 = I0, gamma = gamma, beta = beta, tmax = tmax)
    
    return(result)
  })
  
  output$plot.ui <- renderUI({
    plotOutput("plot", width = paste0(input$PlotWidth, "%"), height = 500)
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
  
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  tags$head(
    tags$style(HTML("
                    img {
                    max-height: 90px;
                    max-width: '100%';
                    }
                    
                    body {
                    background-color: #fff;
                    }
                    "))
    ),
  
  div( includeHTML("www/header.html"), align = "center"),
  h1('ID Dynamics Introduction App', align = "center", style = "background-color:#123c66; color:#fff"),
  
  
  div( style="text-align:center", actionButton("exitBtn", "Exit App") ),
  tags$hr(),
  
  
  # Inputs go at top above output
  # Each input uses either a slider or a text box except for the submit button
  fluidRow(
    column(4,
           sliderInput("PopSize", "Initial Population Size", min = 1000, max = 5000, value = 1000, step = 500)
    ),
    column(4,
           sliderInput("I0", "Initial number of infected hosts", min = 0, max = 100, value = 0, step = 1)
    ),
    column(4,
           sliderInput("tmax", "Maximum simulation time (days)", min = 10, max = 1000, value = 300, step = 10)
    ),
    align = "center"
  ), #close fluidRow structure for input
  
  fluidRow(
    column(6,
           sliderInput("beta", "Rate of transmission (beta, 1/days)", min = 0, max = 0.01, value = 0, step = 0.0001, sep ='')
    ),
    column(6,
           sliderInput("gamma", "Rate at which a host leaves the infectious compartment (gamma, 1/days)", min = 0, max = 2, value = 0.5, step = 0.1)
    ),
    align = "center"
  ), #close fluidRow structure for input
  
  div( style="text-align:center", actionButton("submitBtn", "Run Simulation")  ),
  tags$hr(),
  
  h2('Simulation Results'),
  fluidRow(
    column(12, 
           uiOutput("plot.ui")
    ),
    column(7, 
           # PLaceholder for results of type text
           htmlOutput(outputId = "text"),
           # Placeholder for any possible warning or error messages (this will be shown in red)
           htmlOutput(outputId = "warn")
    ),
    
    column(4, 
           # Slider to change the size of the plot
           sliderInput("PlotWidth", "Plot width (%)", min = 30, max = 100, value = 60, step = 5)
    )
  ),
  
  tags$head(tags$style("#warn{color: red;
                               font-style: italic;
                               }")),
  tags$hr(),
  h2('Instructions'),
  
  #use external function to generate all tabs with instruction content
  
  do.call(tabsetPanel,generate_instruction_tabs()),
  
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage, i.e. the UI part of the app

shinyApp(ui = ui, server = server)
