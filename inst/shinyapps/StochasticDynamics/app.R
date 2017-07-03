#This is the Shiny file for the Stochastic Dynamics App


#the main function with all the functionality for the server
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed
refresh <- function(input, output){

    res <- reactive({
    input$submitBtn
    
    # Read all the input values from the UI
    S0 = isolate(input$S0);
    I0 = isolate(input$I0);
    tmax = isolate(input$tmax);

    bP = isolate(input$bP);
    bI = isolate(input$bI);

    gP = isolate(input$gP);
    gI = isolate(input$gI);

    w = isolate(input$w);

    m = isolate(input$m)
    n = isolate(input$n);
    nreps = isolate(input$nreps)

    # Call the adaptivetau simulator with the given parameters
    # simulation will be run multiple times based on value of nreps
    #result is returned as list
    result <- list()
    for (nn in 1:nreps)
    {
     result[[nn]] <- simulate_stochastic(S0 = S0, I0 = I0, tmax = tmax, bP = bP, bI = bI, gP = gP, gI = gI, w = w, m = m, n = n)
    }
    
    return(result)
    })
    
    #function that takes result saved in res and produces output
    #output (plots, text, warnings) is stored in and modifies the global variable output
    generate_simoutput(input,output,res)
    
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

} #ends main/outer shiny server function


#This is the UI for the Stochastic Dynamics App
ui <- fluidPage(
  includeCSS("../styles/shinystyle.css"),
  
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  h1('Stochastic Dynamics App', align = "center", style = "background-color:#123c66; color:#fff"),
  
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
                    sliderInput("S0", "initial number of susceptible hosts (S0)", min = 100, max = 3000, value = 1000, step = 50)
             ),
             column(4,
                    sliderInput("I0", "initial number of symptomatic hosts (I0)", min = 0, max = 500, value = 1, step = 1)
             ),
             column(4,
                    sliderInput("tmax", "Maximum simulation time (tmax)", min = 1, max = 1200, value = 100, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("bP", "Rate of transmission of presymptomatic hosts (bP)", min = 0, max = 0.02, value = 0, step = 0.0001 , sep ='')
             ),
             column(4,
                    sliderInput("bI", "Rate of transmission of symptomatic hosts (bI)", min = 0, max = 0.02, value = 0.01, step = 0.0001 , sep ='')
             ),
             column(4,
                    sliderInput("w", "Rate of immunity loss (w)", min = 0, max = 0.5, value = 0.0, step = 0.01, sep ='')
             )
           ), #close fluidRow structure for input
           
           fluidRow(
             column(6,
                    sliderInput("gP", "Rate at which presymptomatic hosts leave compartment (gP)", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(6,
                    sliderInput("gI", "Rate at which symptomatic hosts leave compartment (gI)", min = 0, max = 5, value = 0.5, step = 0.1)
             )
            ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("m", "Rate of new births (m)", min = 0, max = 10000, value = 0, step = 100)
             ),
             column(4,
                    sliderInput("n", "Natural death rate (n)", min = 0, max = 1, value = 0, step = 0.1)
             ),          
             column(4,
                         sliderInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1)
              )
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
) #end fluidpage

shinyApp(ui = ui, server = server)

