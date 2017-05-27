############################################################
#This is the Shiny file for the Host Heterogeneity App
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
    S10 = isolate(input$S10);
    I10 = isolate(input$I10);
    S20 = isolate(input$S20);
    I20 = isolate(input$I20);
    tmax = isolate(input$tmax);
    
    b11 = isolate(input$b11);
    b21 = isolate(input$b21);
    b12 = isolate(input$b12);
    b22 = isolate(input$b22);
    
    g1 = isolate(input$g1);
    g2 = isolate(input$g2);
    
    w1 = isolate(input$w1);
    w2 = isolate(input$w2);
    
    print(environment()) ################################# Debugging line
    print(ls()) ################################# Debugging line
    
    # Call the ODE solver with the given parameters
    result <- simulate_heterogeneity(S10 = S10, I10 = I10, S20 = S20, I20 = I20, tmax = tmax, b11 = b11, b12 = b12, b21 = b21, b22 = b22, g1 = g1 , g2 = g2, w1 = w1, w2 = w2)
    
    return(list(result)) #this is returned as the res variable
  })
  
  #if we want certain variables plotted and reported separately, we can specify them manually as a list
  #if nothing is specified, all variables are plotted and reported at once
  varlist = list(c("S1","I1","R1"),c("S2","I2","R2") )
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
  h1('Host Heterogeneity App', align = "center", style = "background-color:#123c66; color:#fff"),
  
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
           p('All parameters are assumed to be in units of (inverse) months'),
           fluidRow(
             column(6,
                    sliderInput("S10", "initial number of susceptible type 1 hosts", min = 100, max = 5000, value = 1000, step = 100)
             ),
             column(6,
                    sliderInput("I10", "initial number of infected type 1  hosts", min = 0, max = 100, value = 0, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("S20", "initial  number of susceptible type 2 hosts", min = 0, max = 5000, value = 0, step = 100)
             ),
             column(6,
                    sliderInput("I20", "initial  number of infected type 2 hosts", min = 0, max = 100, value = 0, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("b11", "Rate of transmission between type 1  hosts", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(6,
                    sliderInput("b22", "Rate of transmission between type 2 hosts", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("b12", "Rate of transmission from type 2 to type 1", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(6,
                    sliderInput("b21", "Rate of transmission from type 1 to type 2", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("g1", "Rate at which infected type 1 hosts leave compartment", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(6,
                    sliderInput("g2", "Rate at which infected type 2 hosts leave compartment", min = 0, max = 5, value = 0.5, step = 0.1)
             )
           ), #close fluidRow structure for input
           
           
           fluidRow(
             column(4,
                    sliderInput("w1", "Rate of waning immunity of type 1", min = 0, max = 5, value = 0, step = 0.05)
             ),
             column(4,
                    sliderInput("w2", "Rate of waning immunity of type 2", min = 0, max = 5, value = 0, step = 0.05)
             ),
             column(4,
                    sliderInput("tmax", "Maximum simulation time (months)", min = 1, max = 1200, value = 100, step = 1)
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
  
) #end fluidpage function, i.e. the UI part of the app

shinyApp(ui = ui, server = server)
