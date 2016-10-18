############################################################
#This is the Shiny file for the Vector Transmission App
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
    Sh0 = isolate(input$Sh0);
    Ih0 = isolate(input$Ih0);
    Sv0 = isolate(input$Sv0);
    Iv0 = isolate(input$Iv0);
    tmax = isolate(input$tmax);
    
    b1 = isolate(input$b1);
    b2 = isolate(input$b2);
    b  = isolate(input$b);
    n  = isolate(input$n);
    g  = isolate(input$g);
    w  = isolate(input$w);
    
    
    # Call the ODE solver with the given parameters
    result <- simulate_vectortransmission(Sh0 = Sh0, Ih0 = Ih0, Sv0 = Sv0, Iv0 = Iv0, tmax = tmax, b1 = b1, b2 = b2, b = b, n = n, g = g, w = w)
    
    return(list(result)) #this is returned as the res variable
  })
  
  #if we want certain variables plotted and reported separately, we can specify them manually as a list
  #if nothing is specified, all variables are plotted and reported at once
  varlist = list(c("Sh","Ih","Rh"), c("Sv",'Iv'))
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
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  #specify name of App below, will show up in title
  h1('Vector Transmission App', align = "center", style = "background-color:#123c66; color:#fff"),
  
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
                    sliderInput("Sh0", "initial number of susceptible hosts", min = 1000, max = 5000, value = 1000, step = 500)
             ),
             column(4,
                    sliderInput("Sv0", "initial number of susceptible vectors", min = 1000, max = 5000, value = 1000, step = 500)
             ),
             column(4,
                    sliderInput("Ih0", "initial number of infected hosts", min = 0, max = 100, value = 0, step = 1)
             ),
             align = "center"
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("Iv0", "initial number of infected vectors", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    sliderInput("tmax", "Maximum simulation time", min = 1, max = 500, value = 100, step = 1)
             ),
             column(4,
                    sliderInput("b1", "vector to host transmission rate", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             align = "center"
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("b2", "host to vector transmission rate", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(4,
                    sliderInput("w", "Rate of waning immunity", min = 0, max = 1, value = 0, step = 0.01, sep ='')
             ),
             column(4,
                    sliderInput("g", "Rate of recovery of infected hosts", min = 0, max = 2, value = 0.5, step = 0.1)
             ),
             align = "center"
           ), #close fluidRow structure for input
           
           fluidRow(
               column(6,
                    sliderInput("b", "Monthly rate of new vector births", min = 0, max = 10000, value = 0, step = 100)
             ),
             column(6,
                    sliderInput("n", "Natural vector death rate", min = 0, max = 2, value = 0, step = 0.01, sep ='')
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