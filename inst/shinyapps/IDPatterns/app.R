############################################################
#This is the Shiny file for the ID Patterns App
#written by Andreas Handel and Sina Solaimanpour  
#maintained by Andreas Handel (ahandel@uga.edu)
#last updated 10/13/2016
############################################################

#the server-side function with the main functionality
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
    
    return(list(result))
  })
  
  #function that takes result saved in res and produces output
  #output (plots, text, warnings) is stored in and modifies the global variable output
  generate_simoutput(input,output,res)
  
} #ends the 'refresh' shiny server function that runs the simulation and returns output

#main shiny server function
server <- function(input, output, session) {
  
  # Waits for the Exit Button to be pressed to stop the app and return to main menu
  observeEvent(input$exitBtn, {
    input$exitBtn
    stopApp(returnValue = 0)
  })
  
  # This function is called to refresh the content of the Shiny App
  refresh(input, output)
  
  # Event handler to listen for the webpage and see when it closes.
  # Right after the window is closed, it will stop the app server and the main menu will
  # continue asking for inputs.
  session$onSessionEnded(function(){
    stopApp(returnValue = 0)
  })
} #ends the main shiny server function


#This is the UI part of the shiny App
ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  #specify name of App below, will show up in title
  h1('ID Patterns App', align = "center", style = "background-color:#123c66; color:#fff"),
  
  #section to add buttons
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
                    sliderInput("PopSize", "Population Size", min = 1000, max = 5000, value = 1000, step = 500)
             ),
             column(4,
                    sliderInput("P0", "Initial number of presymptomatic hosts", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    sliderInput("tmax", "Maximum simulation time (months)", min = 6, max = 12000, value = 120, step = 12)
             ),
             align = "center"
           ), #close fluidRow structure for input
           
           fluidRow(
             column(4,
                    sliderInput("bP", "Level/Rate of transmission by presymptomatic hosts (bP, 1/months)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(4,
                    sliderInput("bA", "Level/Rate of transmission by asymptomatic hosts (bA, 1/months)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(4,
                    sliderInput("bI", "Level/Rate of transmission by symptomatic hosts (bI, 1/months)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             align = "center"
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("gP", "Rate at which presymptomatic hosts leave compartment (gP, 1/months)", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(4,
                    sliderInput("gA", "Rate at which asymptomatic hosts leave compartment (gA, 1/months)", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(4,
                    sliderInput("gI", "Rate at which symptomatic hosts leave compartment (gI, 1/months)", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("f", "Fraction of asymptomatic infections (f)", min = 0, max = 1, value = 0, step = 0.1)
             ),
             column(4,
                    sliderInput("d", "Fraction of deaths in symptomatic hosts (d)", min = 0, max = 1, value = 0, step = 0.1)
             ),
             column(4,
                    sliderInput("w", "Rate of immunity loss (w, 1/months)", min = 0, max = 0.5, value = 0.0, step = 0.01 , sep ='')
             ),
             align = "center"
           ),
           fluidRow(
             column(4,
                    sliderInput("lambda", "Monthly rate of new births (lambda)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    sliderInput("n", "Natural death rate (n, 1/months)", min = 0, max = 0.02, value = 0, step = 0.0005, sep ='')
             ),
             column(4,
                    sliderInput("sigma", "Strength of seasonal variation of transmission (sigma)", min = 0, max = 1, value = 0, step = 0.1)
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
  
) #end fluidpage function, i.e. the UI part of the app

shinyApp(ui = ui, server = server)