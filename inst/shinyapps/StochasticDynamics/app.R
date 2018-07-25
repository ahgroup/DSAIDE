############################################################
#This is the Shiny file for the Stochastic Dynamics App
#written by Andreas Handel, with contributions from others 
#maintained by Andreas Handel (ahandel@uga.edu)
#last updated 7/13/2017
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

    bP = isolate(input$bP);
    bI = isolate(input$bI);

    gP = isolate(input$gP);
    gI = isolate(input$gI);

    w = isolate(input$w);

    m = isolate(input$m)
    n = isolate(input$n);
    nreps = isolate(input$nreps)
    plotscale = isolate(input$plotscale)

# Call the adaptivetau simulator with the given parameters
# simulation will be run multiple times based on value of nreps
#result is returned as list
#save all results to a list for processing plots and text
    
    listlength = 1; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results
    dat = NULL
    
  for (nn in 1:nreps)
    
                  {
     # Adding a progress message 
       withProgress(message = 'Running Simulation', value = 0, {  
             
              simresult <- simulate_stochastic(S0 = S0, I0 = I0, tmax = tmax, bP = bP, 
                                                       bI = bI, gP = gP, gI = gI, w = w, m = m, n = n)
           }) # progress message ends here
    
    # colnames(simresult)[1] = 'xvals' #rename time to xvals for consistent plotting
                                       #reformat data to be in the right format for plotting
    # datnew = tidyr::gather(as.data.frame(simresult), -xvals, value = "yvals", key = "varnames")
    # datnew$IDvar = paste(datnew$varnames,nn,sep='')    #trying to make a variable for plotting same color 
                                                        #lines for each run in ggplot2. doesn't work yet.
    # dat = rbind(dat, datnew)
    dat <- simresult$ts
    
    
    }
    
#code variable names as factor and level them so they show up right in plot
    # mylevels = unique(dat$varnames)
    # dat$varnames = factor(dat$varnames, levels = mylevels)
    
    
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
    
#set min and max for scales. If not provided ggplot will auto-set
    result[[1]]$ymin = 1e-12
    result[[1]]$ymax = max(simresult$ts)
    result[[1]]$xmin = 1e-12
    result[[1]]$xmax = tmax
    
    result[[1]]$xscale = 'identity'
    result[[1]]$yscale = 'identity'
    
    if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'; result[[1]]$xmin = 1e-6}
    if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'; result[[1]]$ymin = 1e-2}
    
#the following are for text display for each plot
    result[[1]]$maketext = TRUE #if true we want the generate_text function to process data and generate text,
                                #if 0 no result processing will occur insinde generate_text
    result[[1]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which will 
                                                 #be passed through to generate_text and displayed for each plot
    
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
                    numericInput("bP", "Rate of transmission of presymptomatic hosts (bP)", min = 0, max = 0.02, value = 0, step = 0.0001  )
             ),
             column(4,
                    numericInput("bI", "Rate of transmission of symptomatic hosts (bI)", min = 0, max = 0.02, value = 0.01, step = 0.0001  )
             ),
             column(4,
                    numericInput("w", "Rate of immunity loss (w)", min = 0, max = 0.5, value = 0.0, step = 0.01 )
             )
           ), #close fluidRow structure for input
           
        fluidRow(
             column(4,
                    numericInput("gP", "Rate at which presymptomatic hosts leave compartment (gP)", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(4,
                    numericInput("gI", "Rate at which symptomatic hosts leave compartment (gI)", min = 0, max = 5, value = 0.5, step = 0.1)
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

