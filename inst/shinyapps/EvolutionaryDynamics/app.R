############################################################
#This is the Shiny file for the Evolutionary Dynamics App
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
    Iu0 = isolate(input$Iu0);
    It0 = isolate(input$It0);
    Ir0 = isolate(input$Ir0);
    tmax = isolate(input$tmax);

    bu = isolate(input$bu);
    bt = isolate(input$bt);
    br = isolate(input$br);

    cu = isolate(input$cu);
    ct = isolate(input$ct);

    f = isolate(input$f);
    
    gu = isolate(input$gu);
    gt = isolate(input$gt);
    gr = isolate(input$gr);
    
    nreps = isolate(input$nreps)
    plotscale = isolate(input$plotscale) # Change the scale of axis interactively 
    
#save all results to a list for processing plots and text
    
    listlength = 1; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results
    
#shows a 'running simulation' message
    
   withProgress(message = 'Running Simulation', value = 0,
                 {
          simresult <- simulate_evolution(S0 = S0, Iu0 = Iu0, It0 = It0, Ir0 = Ir0, tmax = tmax, 
                                            bu = bu, bt = bt, br = br, cu = cu, ct = ct, f = f, gu = gu, gt = gt, gr = gr)
                   
          })
    
#reformat data to be in the right format for plotting
#each plot/text output is a list entry with a data frame in form xvals, yvals, extra variables for stratifications for each plot
    
    dat = tidyr::gather(simresult$ts, -xvals, value = "yvals", key = "varnames")
    
 #code variable names as factor and level them so they show up right in plot
    
    mylevels = unique(dat$varnames)
    dat$varnames = factor(dat$varnames, levels = mylevels)
    
#data for plots and text
#each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
#each variable listed in varnames will also be processed to produce text
    
    result[[1]]$dat = dat
    
#Meta-information for each plot
    
    result[[1]]$plottype = "Lineplot"
    result[[1]]$xlab = "Time"
    result[[1]]$ylab = "Numbers"
    result[[1]]$legend = "Compartments"
    
    result[[1]]$xscale = 'identity'
    result[[1]]$yscale = 'identity'
    if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'}
    if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'}
    
    
#set min and max for scales. If not provided ggplot will auto-set
    
    result[[1]]$ymin = 1e-12
    result[[1]]$ymax = max(simresult$ts)
    result[[1]]$xmin = 1e-12
    result[[1]]$xmax = tmax
    
#the following are for text display for each plot
    
    result[[1]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
    result[[1]]$showtext = '' #text can be added here which will be passed through to generate_text and displayed for each plot
    result[[1]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which will be passed through to generate_text and displayed for each plot
    
    return(result)
    
    })          #ends inner shiny server function that runs the simulation and returns output
    
    
#functions below take result saved in reactive expression result and produce output
#to produce figures, the function generate_plot is used
#function generate_text produces text
#data needs to be in a specific structure for processing
#see information for those functions to learn how data needs to look like
#output (plots, text) is stored in reactive variable 'output'
    
  output$plot  <- renderPlot({
              input$submitBtn
             res = isolate(result())                  #list of all results that are to be turned into plots
           generate_plots(res)                    #create plots with a non-reactive function
       }, width = 'auto', height = 'auto'
    )                                           #finish render-plot statement
    
  output$text <- renderText({
           input$submitBtn
          res = isolate(result())      #list of all results that are to be turned into plots
         generate_text(res)         #create text for display with a non-reactive function
     })
    
   }             #ends the 'refresh' shiny server function that runs the simulation and returns output   


#main shiny server function

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
  h1('Evolutionary Dynamics App', align = "center", style = "background-color:#123c66; color:#fff"),
  
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
                    numericInput("Iu0", "initial number of untreated wild-type infected hosts (Iu0)", min = 0, max = 100, value = 1, step = 1)
             ),
             column(4,
                    numericInput("tmax", "Maximum simulation time (tmax)", min = 1, max = 1200, value = 100, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    numericInput("It0", "initial number of treated wild-type infected hosts (It0)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    numericInput("Ir0", "initial number of resistant infected hosts (Ir0)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    numericInput("bu", "Rate of transmission of untreated wild-type hosts (bu)", min = 0, max = 0.02, value = 0.001, step = 0.0001  )
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    numericInput("bt", "Rate of transmission of treated wild-type hosts (bt)", min = 0, max = 0.02, value = 0, step = 0.0001  )
             ),
             column(4,
                    numericInput("br", "Rate of transmission of resistant hosts (br)", min = 0, max = 0.02, value = 0, step = 0.0001  )
             ),
             column(4,
                    numericInput("cu", "Fraction of resistant generation by untreated hosts (cu)", min = 0, max = 0.5, value = 0.0, step = 0.005 )
             )
           ), #close fluidRow structure for input
           
           fluidRow(
             column(4,
                    numericInput("ct", "Fraction of resistant generation by treated hosts (ct)", min = 0, max = 0.5, value = 0.0, step = 0.005 )
             ),
             column(4,
                    numericInput("gu", "Rate at which untreated hosts leave compartment (gu)", min = 0, max = 5, value = 0.5, step = 0.05)
             ),
             column(4,
                    numericInput("gt", "Rate at which treated hosts leave compartment (gt)", min = 0, max = 5, value = 0.5, step = 0.05)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    numericInput("gr", "Rate at which resistant hosts leave compartment (gr)", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(4,
                    numericInput("f", "Fraction of infected receiving treatment (f)", min = 0, max = 1, value = 0.0, step = 0.05)
             ),
             column(4,
                         numericInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1)
              )
            ),  #close fluidRow structure for input
           
           fluidRow(
             column(4,align = "left",
                    selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
                 )
             
             )  
           
        ),         #end sidebar column for inputs
    
     
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
           
      )         #end main panel column with outcomes
  ),        #end layout with side and main panel
  
  #################################

  #Instructions section at bottom as tabs

     h2('Instructions'),
     do.call(tabsetPanel,generate_instruction_tabs()),  #use external function to generate all tabs with instruction content
    div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
    )    #end fluidpage

shinyApp(ui = ui, server = server)

