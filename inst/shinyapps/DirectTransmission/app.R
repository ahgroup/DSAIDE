############################################################
#This is the Shiny file for the Direct Transmission App
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
    I0 = isolate(input$I0);
    tmax = isolate(input$tmax);
    g = isolate(input$g);
    bd= isolate(input$bd);
    bf= isolate(input$bf);
    m = isolate(input$m);
    n = isolate(input$n);
    w = isolate(input$w);
    A = isolate(input$A);
    scenario = as.numeric(isolate(input$scenario));
    
    
    # Call the ODE solver with the given parameters
    
    result <- simulate_directtransmission(PopSize = PopSize, I0 = I0, tmax = tmax, scenario = scenario, bd = bd, bf = bf, A = A, m = m, n = n, g = g, w = w)

    return(list(result)) #this is returned as the res variable
  })
  
  #function that takes result saved in res and produces output
  #output (plots, text, warnings) is stored in and modifies the global variable 'output'
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
  h1('Modes of Direct Transmission App', align = "center", style = "background-color:#123c66; color:#fff"),
  
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
                    sliderInput("PopSize", "Population Size", min = 1000, max = 5000, value = 1000, step = 500)
             ),
             column(6,
                    sliderInput("I0", "initial number of infected hosts", min = 0, max = 100, value = 0, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("tmax", "Maximum simulation time", min = 1, max = 500, value = 100, step = 1)
             ),
             column(6,
                    selectInput("scenario", "Transmission scenario:",c("density dependent" = 1, 'frequency dependent' = 2))
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("bf", "frequency dependent transmission rate (bf)", min = 0, max = 10, value = 0, step = 0.1 , sep ='')
             ),
             column(6,
                    sliderInput("bd", "density dependent transmission rate (bd)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("A", "Area where hosts interact (for density dependent transmission, arbitrary units)", min = 0.1, max = 10, value = 1, step = 0.1)
             ),
             column(6,
                    sliderInput("g", "Rate of recovery of infected hosts (g)", min = 0, max = 10, value = 1, step = 0.1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("w", "Rate of immunity loss (w)", min = 0, max = 0.1, value = 0.0, step = 0.01 , sep ='')
             ),
             column(4,
                    sliderInput("m", "Rate of new births (m)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    sliderInput("n", "Natural death rate (n)", min = 0, max = 0.02, value = 0, step = 0.0005, sep ='')
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