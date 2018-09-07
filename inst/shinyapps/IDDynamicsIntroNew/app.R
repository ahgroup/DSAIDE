server <- function(input, output, session)
{
    #load model from Rdata file, needs to be in same folder
    currentdir = getwd()
    rdatafile = list.files(path = currentdir, pattern = "\\.Rdata$")
    load(rdatafile)
  
    makeui(model, output) #make the UI for the model

    ###########################################
    #server part that listens for exit button click
    observeEvent(input$exitBtn, {
        input$exitBtn
        stopApp(returnValue = NULL)
    })


    ###########################################
    #server part that listens for run simulation button click
    #on click, run simulation and update plots and text
    observeEvent(input$submitBtn,
    {
        #save all results to a list for processing plots and text
        listlength = 1
        #here we do all simulations in the same figure
        result = vector("list", listlength) #create empty list of right size for results

        #parses the model and creates the code to call/run the simulation
        fctcall <- make_fctcall(input=input,model=model,modeltype='desolve')

        #run simulation, show a 'running simulation' message
        withProgress(message = 'Running Simulation', value = 0,
        {
             eval(parse(text = fctcall)) #execute function
        })

        
        #data for plots and text
        #needs to be in the right format to be passed to generate_plots and generate_text
        #see documentation for those functions for details
        result[[1]]$dat = simresult$ts
        
        #Meta-information for each plot
        result[[1]]$plottype = "Lineplot"
        result[[1]]$xlab = "Time"
        result[[1]]$ylab = "Numbers"
        result[[1]]$legend = "Compartments"
        
        result[[1]]$xscale = 'identity'
        result[[1]]$yscale = 'identity'
        #if (plotscale == 'x' | plotscale == 'both') { result[[1]]$xscale = 'log10'}
        #if (plotscale == 'y' | plotscale == 'both') { result[[1]]$yscale = 'log10'}
        
        #the following are for text display for each plot
        result[[1]]$maketext = TRUE #if true we want the generate_text function to process data and generate text, if 0 no result processing will occur insinde generate_text
        result[[1]]$showtext = '' #text can be added here which will be passed through to generate_text and displayed for each plot
        result[[1]]$finaltext = 'Numbers are rounded to 2 significant digits.' #text can be added here which will be passed through to generate_text and displayed for each plot

        #create plot from results
        output$plot  <- renderPlot({
             generate_plots(result)
        }, width = 'auto', height = 'auto')

        #create text from results
        output$text <- renderText({
             generate_text(result)     #create text for display with a non-reactive function
        })
    }) #end ObserveEvent submit button
}  #ends the main shiny server function


ui <- fluidPage(
  includeCSS("../../media/dsaide.css"),
  #add header and title
  withMathJax(),
  # 
  tags$head(tags$style(".myrow{vertical-align: bottom;}")),
  div( includeHTML("../../media/header.html"), align = "center"),
    #UI does not 'know' about the model, all that is processed in the server function and only displayed here
    h1(uiOutput("title"), align = "center", style = "background-color:#123c66; color:#fff"),

    #section to add buttons
    fluidRow(column(
        6,
        actionButton("submitBtn", "Run Simulation", class = "submitbutton")
    ),
    column(
        6,
        actionButton("exitBtn", "Exit App", class = "exitbutton")
    ),
    align = "center"),
    #end section to add buttons

    tags$hr(),

    ################################
    #Split screen with input on left, output on right
    fluidRow(
        #all the inputs in here
        column(
            6,
            h2('Simulation Settings'),
            uiOutput("vars"),
            uiOutput("pars"),
            uiOutput("time")

        ),
        #end sidebar column for inputs

        #all the outcomes here
        column(
            6,

            #################################
            #Start with results on top
            h2('Simulation Results'),
            plotOutput(outputId = "plot", height = "500px"),
            # PLaceholder for results of type text
            htmlOutput(outputId = "text"),
            tags$hr()
        ) #end main panel column with outcomes
  ), #end layout with side and main panel
  
  #################################
  #Instructions section at bottom as tabs
  h2('Instructions'),
  #use external function to generate all tabs with instruction content
  #browser(),
  do.call(tabsetPanel, generate_documentation() ),
  div(includeHTML("../../media/footer.html"), align="center", style="font-size:small") #footer
  
) #end fluidpage

shinyApp(ui = ui, server = server)
