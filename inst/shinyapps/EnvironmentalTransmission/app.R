############################################################
#This is the Shiny file for the Environmental Transmission App
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
    S0 = isolate(input$S0);
    I0 = isolate(input$I0);
    E0 = isolate(input$E0);
    tmax = isolate(input$tmax);
    g  = isolate(input$g);
    bd = isolate(input$bd);
    be = isolate(input$be);
    m  = isolate(input$m);
    n  = isolate(input$n);
    c  = isolate(input$c);
    p  = isolate(input$p);
    
    
    # Call the ODE solver with the given parameters
    result <- simulate_environmentaltransmission(S = S0, I = I0, E = E0, tmax = tmax, bd = bd, be = be, m = m, n = n, g = g, p = p, c = c)
    
    return(list(result)) #this is returned as the res variable
  })
  
  #if we want certain variables plotted and reported separately, we can specify them manually as a list
  #if nothing is specified, all variables are plotted and reported at once
  varlist = list(c("S","I","R"), c("E"))
  #function that takes result saved in res and produces output
  #output (plots, text, warnings) is stored in and modifies the global variable 'output'
  generate_simoutput(input,output,res,varlist=varlist)
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
  
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  
  div( includeHTML("www/header.html"), align = "center"),
  h1('Environmental Transmission App', align = "center", style = "background-color:#123c66; color:#fff"),
  
  #################################
  #end section to add buttons
  fluidRow(
    column(6,
           div( style="text-align:center", actionButton("submitBtn", "Run Simulation", style="color: #000000; background-color: #D2FFE2")  )
    ),
    column(6,
           div( style="text-align:center", actionButton("exitBtn", "Exit App", style="color: #000000; background-color: #BDCCD9") )
    )
    
  ), #end section to add buttons
  
  tags$hr(),
  
  
  #################################
  #Split screen with input on left, output on right
  fluidRow(
    #all the inputs in here
    column(6,
           #################################
           # Inputs section
           h2('Simulation Settings'),
           fluidRow(
             column(6,
                    sliderInput("S0", "initial number of susceptible hosts", min = 1000, max = 5000, value = 1000, step = 500)
             ),
             column(6,
                    sliderInput("I0", "initial number of infected hosts", min = 0, max = 50, value = 0, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("E0", "initial amount of environmental pathogen", min = 0, max = 100, value = 0, step = 1)
             ),
             column(6,
                    sliderInput("tmax", "Maximum simulation time", min = 1, max = 500, value = 100, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("bd", "direct transmission rate (bd)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(6,
                    sliderInput("be", "environmental transmission rate (be)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("p", "Rate of pathogen shedding by infected hosts (p)", min = 0, max = 50, value = 1, step = 0.1)
             ),
             column(6,
                    sliderInput("c", "Rate of environmental pathogen decay (c) ", min = 0, max = 10, value = 0, step = 0.01 , sep ='')
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("g", "Rate of recovery of infected hosts (g)", min = 0, max = 2, value = 0.5, step = 0.1)
             ),
             column(4,
                    sliderInput("m", "Rate of new births (m)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    sliderInput("n", "Natural death rate (n)", min = 0, max = 0.02, value = 0, step = 0.0005, sep ='')
             )
           ) #close fluidRow structure for input
    ), #end sidebar column for inputs
    
    #all the outcomes here
    column(6,
           
           #################################
           #Start with results on top
           h2('Simulation Results'),
           plotOutput(outputId = "plot"),
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
  #Ends the 2 column structure with inputs on left and outputs on right
  
  
  
  #################################
  #Instructions section at bottom as tabs
  h2('Instructions'),
  
  #use external function to generate all tabs with instruction content
  do.call(tabsetPanel,generate_instruction_tabs()),
  
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage



shinyApp(ui = ui, server = server)