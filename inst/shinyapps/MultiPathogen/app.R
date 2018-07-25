############################################################
#This is the Shiny file for the Multiple Pathogen App
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
        I10 = isolate(input$I10);
        I20 = isolate(input$I20);
        I120 = isolate(input$I120);
        
        tmax = isolate(input$tmax);
        
        b1 = isolate(input$b1);
        b2 = isolate(input$b2);
        b12 = isolate(input$b12);
        a  = isolate(input$a);
        
        g1 = isolate(input$g1);
        g2 = isolate(input$g2);
        g12 = isolate(input$g12);
        plotscale = isolate(input$plotscale)
        
        
#save all results to a list for processing plots and text
        
        listlength = 2; #here we do all simulations in the same figure
        result = vector("list", listlength) #create empty list of right size for results
        
# Call the ODE solver with the given parameters
        
        withProgress(message = 'Running Simulation', value = 0, {
          
        simresult <- simulate_multipathogen(S0 = S0, I10 = I10, I20 = I20, I120 = I120, tmax = tmax, b1 = b1, b2 = b2,
                                              b12 = b12, g1 = g1, g2 = g2, g12 = g12, a = a)
        
        })
        
        
#rename time to xvals for consistent plotting
        
      #  colnames(simresult) = c('xvals',"S","I1","I2",'R1','R2',"I1X","I2X","I12",'R12')  
        
        
# reformat data to be in the right format for plotting 
# dat1 store the input used for plot 1
# dat2 store the input used for plot 2
        
      #  dat1 = tidyr::gather(as.data.frame(simresult[,c(1,2:6)]), -xvals, value = "yvals", key = "varnames")
      #  dat2 = tidyr::gather(as.data.frame(simresult[,c(1,7:10)]), -xvals, value = "yvals", key = "varnames")      
        dat1 <- simresult$ts[ , c(1, 2:6)]
        dat2 <- simresult$ts[ , c(1, 7:10)]
        
#code variable names as factor and level them so they show up right in plot   
        # mylevels1 = unique(dat1$varnames)
        # dat1$varnames = factor(dat1$varnames, levels = mylevels1)
        # 
        # mylevels2 = unique(dat2$varnames)
        # dat2$varnames = factor(dat2$varnames, levels = mylevels2)
        
#data for plots and text
#each variable listed in the varnames column will be plotted on the y-axis, with its values in yvals
#each variable listed in varnames will also be processed to produce text
        
        result[[1]]$dat = dat1
        result[[2]]$dat = dat2   
        
        for (i in 1 :2) {
          
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
          
          result[[i]]$ymin = 1e-12
          result[[i]]$ymax = max(result[[i]]$dat)
          result[[i]]$xmin = 1e-12
          result[[i]]$xmax = tmax
          
#the following are for text display for each plot
          
          result[[i]]$maketext = TRUE  #if true we want the generate_text function to process data and generate  
                                       # text,if 0 no result processing will occur insinde generate_text
          result[[i]]$showtext = ''   #text can be added here which will be passed through to generate_text
                                      #and displayed for each plot
          result[[i]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which 
                                     #will be passed through to generate_text and displayed for each plot
          
         }
        
        
        return(result) #this is returned as the res variable
        
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
    h1('Multiple Pathogen App', align = "center", style = "background-color:#123c66; color:#fff"),
    
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
                   column(6,
                          numericInput("S0", "initial number of susceptible hosts (S0)", min = 100, max = 5000, value = 1000, step = 100)
                   ),
                   column(6,
                          numericInput("I10", "initial number of hosts infected with pathogen 1 (I10)", min = 0, max = 100, value = 0, step = 1)
                   )
               ), #close fluidRow structure for input
               fluidRow(
                   column(6,
                          numericInput("I20", "initial number of hosts infected with pathogen 2 (I20)", min = 0, max = 100, value = 0, step = 1)
                   ),
                   column(6,
                          numericInput("I120", "initial number of hosts infected with pathogens 1 and 2 (I120)", min = 0, max = 100, value = 0, step = 1)
                   )
               ), #close fluidRow structure for input
               fluidRow(
                   column(6,
                          numericInput("b1", "Rate at which hosts infected with pathogen 1 transmit (b1)", min = 0, max = 0.01, value = 0, step = 0.0001  )
                   ),
                   column(6,
                          numericInput("b2", "Rate at which hosts infected with pathogen 2 transmit (b2)", min = 0, max = 0.01, value = 0, step = 0.0001  )
                   )
               ), #close fluidRow structure for input
               fluidRow(
                   column(6,
                          numericInput("b12", "Rate at which double infected hosts transmit (b12)", min = 0, max = 0.01, value = 0, step = 0.0001  )
                          ),
                 column(6,
                      numericInput("g1", "Rate at which hosts infected with type 1 recover (g1)", min = 0, max = 5, value = 0.5, step = 0.1)
                        )
               ), #close fluidRow structure for input
               fluidRow(
                   column(6,
                          numericInput("g2", "Rate at which hosts infected with type 2 recover (g2)", min = 0, max = 5, value = 0.5, step = 0.1)
                   ),
                   column(6,
                          numericInput("g12", "Rate at which double infected host recover (g12)",  min = 0, max = 5, value = 0.5, step = 0.1)
                   )
               ), #close fluidRow structure for input
               
               fluidRow(
                   column(6,
                          numericInput("a", "Fraction of pathogen 1 infections produced by double infected (a)", min = 0, max = 1, value = 0.5, step = 0.05)
                   ),
                  
                   column(6,
                          numericInput("tmax", "Maximum simulation time (tmax)", min = 1, max = 1200, value = 100, step = 1)
                   )
                   
                   
               ),#close fluidRow structure for input
               
              fluidRow(
                column(6,
                       selectInput("plotscale", "Log-scale for plot:",c("none" = "none", 'x-axis' = "x", 'y-axis' = "y", 'both axes' = "both"))
                  ) 
              )
               
               #close fluidRow structure for input
               
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