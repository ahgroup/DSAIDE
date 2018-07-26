############################################################
#This is the Shiny file for the Stochastic SIR App
#written by Andreas Handel, with contributions from others 
#maintained by Andreas Handel (ahandel@uga.edu)
#last updated 7/13/2018
############################################################


#the main function with all the functionality for the server
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed
refresh <- function(input, output){

    result <- reactive({
    input$submitBtn
    
# Read all the input values from the UI
    S0 = isolate(input$S0);
    I0 = isolate(input$I0);
    tmax = isolate(input$tmax);
    b = isolate(input$b);
    g = isolate(input$g);
    m = isolate(input$m)
    n = isolate(input$n);
    models = as.numeric(isolate(input$models))
    rngseed = isolate(input$rngseed)
    nreps = isolate(input$nreps)
    plotscale = isolate(input$plotscale)

# Call the adaptivetau simulator with the given parameters
# simulation will be run multiple times based on value of nreps
#result is returned as list
#save all results to a list for processing plots and text
    
    listlength = 1; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results
    
    #show progress bar during simulation run
    withProgress(message = 'Running Simulation', value = 0, {
      
      if (models == 1 | models == 3) #deterministic model
      {
        result_ode <- simulate_introduction(S0 = S0, I0 = I0, g = g, b = b, tmax = tmax)
        result_ode <- result_ode$ts
        colnames(result_ode) = c('xvals','Sdet','Idet','Rdet')
        dat_ode = tidyr::gather(as.data.frame(result_ode), -xvals, value = "yvals", key = "varnames")
        dat_ode$IDvar = dat_ode$varnames
        dat_ode$nreps = 1
      }
      
      # stochastic model
      if (models == 2 | models == 3)
      {
        
        datall = NULL
        # Call the adaptivetau simulator with the given parameters
        # simulation will be run multiple times based on value of nreps
        for (nn in 1:nreps)
        {
          #add number of rep to seed, otherwise it's exactly the same trajectory each time
          
          simresult <- simulate_stochastic_SIR(S0 = S0, I0 = I0, tmax = tmax, b = b, g = g, m = m, n = n, rngseed = rngseed + nn)
          
          simresult <- simresult$ts
          colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
          #reformat data to be in the right format for plotting
          dat = tidyr::gather(as.data.frame(simresult), -xvals, value = "yvals", key = "varnames")
          dat$IDvar = paste(dat$varnames,nn,sep='') #make a variable for plotting same color lines for each run in ggplot2
          dat$nreps = nn
          datall = rbind(datall,dat)
        }
      } #end stochastic model
      
    }) #end progress bar wrapper
    
    #depending on if user wants only 1 model or both
    if (models == 1) { dat = dat_ode}
    if (models == 2) { dat = datall}
    if (models == 3) { dat <- rbind(dat_ode,datall) }
    
    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    result[[1]]$dat = dat
    
    #Meta-information for each plot
    result[[1]]$plottype = "Lineplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legend = "Compartments"
    result[[1]]$linesize = 1
    
  
    
    result[[1]]$xscale = 'identity'
    result[[1]]$yscale = 'identity'
    if (plotscale == 'x' | plotscale == 'both')
    {
      result[[1]]$xscale = 'log10'
    }
    if (plotscale == 'y' | plotscale == 'both')
    {
      result[[1]]$yscale = 'log10'
    }
    
    #the following are for text display for each plot
    result[[1]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
    #the 1st plot can have a field with text that is displayed once at the end of the text block.
    result[[1]]$finaltext = 'For stochastic simulation scenarios, values shown are the mean over all simulations.'
    
    return(result)
    
    })
    
#functions below take result saved in reactive expression result and produce output
#to produce figures, the function generate_plot is used
#function generate_text produces text
#data needs to be in a specific structure for processing
#see information for those functions to learn how data needs to look like
#output (plots, text) is stored in reactive variable 'output'
    
    
    output$plot  <- renderPlot({
      withProgress(message = 'Plot in progress', value = 0, { 
      input$submitBtn
      res=isolate(result())       #list of all results that are to be turned into plots
      generate_plots(res)        #create plots with a non-reactive function
      })
    }, width = 'auto', height = 'auto'
    )   #finish render-plot statement

  
    output$text <- renderText({
      input$submitBtn
      res=isolate(result())         #list of all results that are to be turned into plots
      generate_text(res)         #create text for display with a non-reactive function
    })
    
    
}         #ends the 'refresh' shiny server function that runs the simulation and returns output

server <- function(input, output, session) {

# Waits for the Exit Button to be pressed to stop the app server
  
  observeEvent(input$exitBtn, {
    input$exitBtn
    stopApp(returnValue = NULL)
  })

# This function is called to refresh the content of the UI
  refresh(input, output)

} #ends main/outer shiny server function


#This is the UI for the Stochastic Dynamics App

ui <- fluidPage(
  includeCSS("../styles/dsaide.css"),
  
  #add header and title
   
  div( includeHTML("../styles/header.html"), align = "center"),
  h1('Stochastic SIR App', align = "center", style = "background-color:#123c66; color:#fff"),
  
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
                    numericInput("S0", "initial number of susceptible hosts (S0)", min = 100, max = 3000, value = 1000, step = 50)
             ),
          column(4,
                    numericInput("I0", "initial number of symptomatic hosts (I0)", min = 0, max = 500, value = 1, step = 1)
             ),
          column(4,
                    numericInput("tmax", "Maximum simulation time (tmax)", min = 1, max = 1200, value = 100, step = 1)
             )
           ), #close fluidRow structure for input
        
        fluidRow(
          column(4,
                 numericInput("b", "Rate of transmission (b)", min = 0, max = 0.02, value = 0.01, step = 0.0001  )
          ),
          column(4,
                    numericInput("g", "Rate at which infected hosts leave compartment (g)", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(4,
                    numericInput("m", "Rate of new births (m)", min = 0, max = 10000, value = 0, step = 100)
             )
             
            ), #close fluidRow structure for input
        fluidRow(
             column(4,
                    numericInput("n", "Natural death rate (n)", min = 0, max = 1, value = 0, step = 0.1)
             ),          
             column(4,
                    numericInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1)
              ),
             column(4,
                  selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
             ),
             fluidRow(
               column(6,
                      selectInput("models", "Models to run",c("deterministic" = 1, 'stochastic' = 2, 'both' = 3), selected = '1')
               ),       
               column(6,
                      numericInput("rngseed", "Random number seed", min = 1, max = 1000, value = 123, step = 1)
                      
               ),
               align = "center"
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
  # do.call(tabsetPanel,generate_instruction_tabs()),
  do.call(tabsetPanel, generate_documentation()),
  div(includeHTML("../styles/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

shinyApp(ui = ui, server = server)

