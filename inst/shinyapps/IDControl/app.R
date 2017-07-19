############################################################
#This is the Shiny file for the ID Control App
#written by Andreas Handel and Sina Solaimanpour 
#maintained by Andreas Handel (ahandel@uga.edu)
#last updated 7/13/2017
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
    Sv0 = isolate(input$Sv0);
    Iv0 = isolate(input$Iv0);
    tmax = isolate(input$tmax);
    
    bP = isolate(input$bP);
    bA = isolate(input$bA);
    bI = isolate(input$bI);
    bE = isolate(input$bE);
    bv = isolate(input$bv);
    bh = isolate(input$bh);
    
    gP = isolate(input$gP);
    gA = isolate(input$gA);
    gI = isolate(input$gI);
    pA = isolate(input$pA);
    pI = isolate(input$pI);
    
    c = isolate(input$c);
    f = isolate(input$f);
    d = isolate(input$d);
    w = isolate(input$w);
    
    mh = isolate(input$mh);
    nh = isolate(input$nh);
    mv = isolate(input$mv);
    nv = isolate(input$nv);
    
    # Call the ODE solver with the given parameters
    result <- simulate_idcontrol(S0 = S0, I0 = I0, E0 = E0, Sv0 = Sv0, Iv0 = Iv0, tmax = tmax, bP = bP, bA = bA, bI = bI, bE = bE, bv = bv, bh = bh, gP = gP , gA = gA, gI = gI, pA = pA, pI = pI, c = c, f = f, d = d, w = w, mh = mh, nh = nh, mv = mv, nv = nv)
    
    return(list(result)) #this is returned as the res variable
  })
  
  #if we want certain variables plotted and reported separately, we can specify them manually as a list
  #if nothing is specified, all variables are plotted and reported at once
  varlist = list(c('S','P','A','I','R','D'),c('Sv','Iv'),c('E'))
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
  includeCSS("../styles/dsaide.css"),
  #add header and title
   
  div( includeHTML("www/header.html"), align = "center"),
  #specify name of App below, will show up in title
  h1('ID Control App', align = "center", style = "background-color:#123c66; color:#fff"),
  
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
                    numericInput("S0", "initial number of susceptible hosts (S0)", min = 100, max = 5000, value = 1000, step = 100)
             ),
             column(4,
                    numericInput("I0", "initial number of symptomatic hosts (I0)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    numericInput("E0", "initial amount of environmental pathogen (E0)", min = 0, max = 5000, value = 0, step = 100)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    numericInput("Sv0", "initial number of susceptible vectors (Sv0)", min = 0, max = 5000, value = 0, step = 100)
             ),
             column(4,
                    numericInput("Iv0", "initial number of infected vectors (Iv0)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    numericInput("tmax", "Maximum simulation time (tmax)", min = 1, max = 1200, value = 100, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    numericInput("bP", "Rate of transmission from pre-symptomatic hosts (bP)", min = 0, max = 0.01, value = 0, step = 0.0001  )
             ),
             column(4,
                    numericInput("bA", "Rate of transmission from asymptomatic hosts (bA)", min = 0, max = 0.01, value = 0, step = 0.0001  )
             ),
             column(4,
                    numericInput("bI", "Rate of transmission from symptomatic hosts (bI)", min = 0, max = 0.01, value = 0, step = 0.0001  )
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    numericInput("bE", "Rate of transmission from environment (bE)", min = 0, max = 0.01, value = 0, step = 0.0001  )
             ),
             column(4,
                    numericInput("bv", "Rate of transmission from vectors (bv)", min = 0, max = 0.01, value = 0, step = 0.0001  )
             ),
             column(4,
                    numericInput("bh", "Rate of transmission to vectors (bh)", min = 0, max = 0.01, value = 0, step = 0.0001  )
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    numericInput("gP", "Rate at which presymptomatic hosts leave compartment (gP)", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(4,
                    numericInput("gA", "Rate at which asymptomatic hosts leave compartment (gA)", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(4,
                    numericInput("gI", "Rate at which symptomatic hosts leave compartment (gI)", min = 0, max = 5, value = 0.5, step = 0.1)
             )
           ), #close fluidRow structure for input
           
           fluidRow(
             column(4,
                    numericInput("pA", "Rate of pathogen shedding by asymptomatic hosts (pA)", min = 0, max = 10, value = 0, step = 0.1)
             ),
             column(4,
                    numericInput("pI", "Rate of pathogen shedding by symptomatic hosts (pI)", min = 0, max = 10, value = 0, step = 0.1)
             ),
             column(4,
                    numericInput("c", "Rate of environmental pathogen decay (c)", min = 0, max = 10, value = 0, step = 0.1  )
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    numericInput("f", "Fraction of hosts that are asymptomatic (f)", min = 0, max = 1, value = 0, step = 0.1)
             ),
             column(4,
                    numericInput("w", "Rate of waning host immunity (w)", min = 0, max = 50, value = 0, step = 0.1)
             ),
             column(4,
                    numericInput("d", "Fraction of symptomatic hosts that die (d)", min = 0, max = 1, value = 0, step = 0.01  )
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    numericInput("mh", "birth rate of hosts (mh)", min = 0, max = 100, value = 0, step = 0.01  )
             ),
             column(6,
                    numericInput("nh", "death rate of hosts (nh)", min = 0, max = 100, value = 0, step = 0.01  )
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    numericInput("mv", "birth rate of vectors (mv)", min = 0, max = 5000, value = 0, step = 1  )
             ),
             column(6,
                    numericInput("nv", "death rate of vectors (nv)", min = 0, max = 30, value = 0, step = 0.1  )
             )
           ) #close fluidRow structure for input
           
    ), #end sidebar column for inputs
    #all the outcomes here
    column(6,
           
           #################################
           #Start with results on top
           h2('Simulation Results'),
           plotOutput(outputId = "plot", height = "1000px"),
           # PLaceholder for results of type text
           htmlOutput(outputId = "text"),
           #Placeholder for any possible warning or error messages (this will be shown in red)
           htmlOutput(outputId = "warn"),
           
           tags$head(tags$style("#warn{color: red;
                                font-style: italic;
                                }"))

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