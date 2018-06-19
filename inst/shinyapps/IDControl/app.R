############################################################
#This is the Shiny file for the ID Control App
#written by Andreas Handel, with contributions from others 
#maintained by Andreas Handel (ahandel@uga.edu)
#last updated 7/13/2017
############################################################

#the server-side function with the main functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed
refresh <- function(input, output){
  
  # This reactive takes the input data and sends it over to the simulator
  # Then it will get the results back and return it as the "res" variable
  result <- reactive({
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
    plotscale = isolate(input$plotscale)
    
#save all results to a list for processing plots and text
    listlength = 3; #here we do all simulations in the same figure
    result = vector("list", listlength) #create empty list of right size for results   
    
    # Call the ODE solver with the given parameters
    # simulation run progress message 
    
    withProgress(message = 'Running Simulation', value = 0, {
    
    simresult <- simulate_idcontrol(S0 = S0, I0 = I0, E0 = E0, Sv0 = Sv0, Iv0 = Iv0, tmax = tmax, bP = bP, bA = bA,
                                 bI = bI, bE = bE, bv = bv, bh = bh, gP = gP , gA = gA, gI = gI, pA = pA, pI = pI, 
                                 c = c, f = f, d = d, w = w, mh = mh, nh = nh, mv = mv, nv = nv)
    
    })
    
    
#rename time to xvals for consistent plotting
    
    colnames(simresult) = c('xvals','S','P','A','I','R','D','Sv','Iv','E')
    
# reformat data to be in the right format for plotting 
# dat1 store the input used for plot 1
# dat2 store the input used for plot 2
    
    dat1 = tidyr::gather(as.data.frame(simresult[,c(1,2:7)]), -xvals, value = "yvals", key = "varnames")
    dat2 = tidyr::gather(as.data.frame(simresult[,c(1,8:9)]), -xvals, value = "yvals", key = "varnames")
    dat3 = tidyr::gather(as.data.frame(simresult[,c(1,10)]), -xvals, value = "yvals", key = "varnames")
    
    
#code variable names as factor and level them so they show up right in plot   
    mylevels1 = unique(dat1$varnames)
    dat1$varnames = factor(dat1$varnames, levels = mylevels1)
    
    mylevels2 = unique(dat2$varnames)
    dat2$varnames = factor(dat2$varnames, levels = mylevels2)
    
    mylevels3 = unique(dat3$varnames)
    dat3$varnames = factor(dat3$varnames, levels = mylevels3)
    
    #data for plots and text
    #each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
    #each variable listed in varnames will also be processed to produce text
    
    result[[1]]$dat = dat1
    result[[2]]$dat = dat2
    result[[3]]$dat = dat3
    
    for (i in 1 : 3) {
      
#Meta-information for each plot
      
      result[[i]]$plottype = "Lineplot"
      result[[i]]$xlab = "Time"
      result[[i]]$ylab = "Numbers"
      result[[i]]$legend = "Compartments"
      
      result[[i]]$xscale = 'identity'
      result[[i]]$yscale = 'identity'
      if (plotscale == 'x' | plotscale == 'both') { result[[i]]$xscale = 'log10'}
      if (plotscale == 'y' | plotscale == 'both') { result[[i]]$yscale = 'log10'}
      
      
#set min and max for scales. If not provided ggplot will auto-set
      
      #set min and max for scales. If not provided ggplot will auto-set
      result[[i]]$ymin = max(1e-10,min(dat$yvals))
      result[[i]]$ymax = min(1e20,max(dat$yvals))
      result[[i]]$xmin = max(1e-10,min(dat$xvals))
      result[[i]]$xmax = max(dat$xvals)
      
#the following are for text display for each plot
      
      result[[i]]$maketext = TRUE  #if true we want the generate_text function to process data and generate  
                                   # text,if 0 no result processing will occur insinde generate_text
      result[[i]]$showtext = ''    #text can be added here which will be passed through to generate_text
                                   #and displayed for each plot
      result[[i]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which 
                                   #will be passed through to generate_text and displayed for each plot
      
    }
    
    return(result) 
    
  })
  
    
#functions below take result saved in reactive expression result and produce output
#to produce figures, the function generate_plot is used
#function generate_text produces text
#data needs to be in a specific structure for processing
#see information for those functions to learn how data needs to look like
#output (plots, text) is stored in reactive variable 'output'
  
  output$plot  <- renderPlot({
        input$submitBtn
        res=isolate(result())             #list of all results that are to be turned into plots
       generate_plots(res)              #create plots with a non-reactive function
    }, width = 'auto', height = 'auto'
  )                                       #finish render-plot statement
  
  output$text <- renderText({
      input$submitBtn
      res=isolate(result())     #list of all results that are to be turned into plots
      generate_text(res)         #create text for display with a non-reactive function
  })
  
  
  
  
 } #ends the 'refresh' shiny server function that runs the simulation and returns output

  
  

#main shiny server function
server <- function(input, output, session) {
 
  
  # Waits for the Exit Button to be pressed to stop the app and return to main menu
  observeEvent(input$exitBtn, {
    input$exitBtn
    stopApp(returnValue = NULL)
  })
  
  # This function is called to refresh the content of the Shiny App
  refresh(input, output)
  
 
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
             column(4,
                    numericInput("mh", "birth rate of hosts (mh)", min = 0, max = 100, value = 0, step = 0.01  )
             ),
             column(4,
                    numericInput("nh", "death rate of hosts (nh)", min = 0, max = 100, value = 0, step = 0.01  )
             ),
             column(4,
                    numericInput("mv", "birth rate of vectors (mv)", min = 0, max = 5000, value = 0, step = 1  )
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    numericInput("mv", "birth rate of vectors (mv)", min = 0, max = 5000, value = 0, step = 1  )
             ),
             column(4,
                    numericInput("nv", "death rate of vectors (nv)", min = 0, max = 30, value = 0, step = 0.1  )
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
           plotOutput(outputId = "plot", height = "1000px"),
           # PLaceholder for results of type text
           htmlOutput(outputId = "text")

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