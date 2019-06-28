#This is a bit of code and instructions for deployment of the package to shinyappsio
#to deploy, follow these steps:
#1. go into the folder where this file (app.R) resides
#2. install the package through CRAN or github if we want to use the github version
#devtools::install_github('ahgroup/DSAIDE')
#3. #uncomment this line of code  
#library('DSAIDE')
#4. with the above 'library' statement active, deploy with:
#run rsconnect::deployApp()
#5. comment out the library command again


##############################################
#This is the Shiny App for the main menu of DSAIDE

#get names of all existing apps
packagename = "DSAIDE"
appdir = system.file("appinformation", package = packagename) #find path to apps
fullappNames = list.files(path = appdir, pattern = "+.settings", full.names = FALSE)
appNames = gsub("_settings.R" ,"",fullappNames)
allsimfctfile = paste0(system.file("simulatorfunctions", package = packagename),"/simulatorfunctions.zip")
currentdocfilename <<- NULL

#this function is the server part of the app
server <- function(input, output, session)
{
  #to get plot engine be object to always be processed
  output$plotengine <- renderText('ggplot')
  outputOptions(output, "plotengine", suspendWhenHidden = FALSE)
  
  #######################################################
  #start code that listens to model selection buttons and creates UI for a chosen model
  #######################################################
  lapply(appNames, function(appName)
  {
    observeEvent(input[[appName]],
    {
      currentapp <<- appName #assign currently chosen app to global app variable
      #file name for documentation
      currentdocfilename <<- paste0(appdir,'/',currentapp,'_documentation.html')
      settingfilename = paste0(appdir,'/',currentapp,'_settings.R')
      
      output$ggplot <- NULL
      output$plotly <- NULL
      output$text <- NULL
      output$floattask <- NULL
      
      #load/source an R settings file that contains additional information for a given app
      #the information is stored in a list called 'appsettings'
      #different models can have different variables
      #all models need the following:
      #variable apptitle - the name of the app
      #variable simfunction - the name of the simulation function(s)
      #variable modeltype - the type of the model to be run or NULL if set by UI
      #additional elements that can be provided:
      #variable otherinputs - contains additional shiny UI elements that are not generated automaticall by functions above
      #for instance all non-numeric inputs need to be provided separately.
      #If not needed, it is NULL
      source(settingfilename) #source the file with additional settings to load them
      
      source(settingfilename) #source the file with additional settings to load them
    
      #extract function and other inputs and turn them into a taglist
      #this uses the 1st function provided by the settings file and stored in currentsimfct
      #indexing sim function in case there are multiple
      
      modelinputs <- generate_shinyinput(mbmodel = appsettings$simfunction[1], otherinputs = appsettings$otherinputs, packagename = packagename)
      output$modelinputs <- renderUI({modelinputs})
            
      #display all inputs and outputs on the analyze tab
      output$analyzemodel <- renderUI({
          tagList(
            tags$div(id = "shinyapptitle", appsettings$apptitle),
            tags$hr(),
            #Split screen with input on left, output on right
            fluidRow(
              column(6,
                h2('Simulation Settings'),
                wellPanel(uiOutput("modelinputs")
                #tags$p(downloadButton(outputId = "download_code", label = "Download Code"), align = "center")  #don't show for now until all works
                          ) #end wellPanel
              ), #end sidebar column for inputs
              column(6,
                h2('Simulation Results'),
                conditionalPanel("output.plotengine == 'ggplot'", shiny::plotOutput(outputId = "ggplot") ),
                conditionalPanel("output.plotengine == 'plotly'", plotly::plotlyOutput(outputId = "plotly") ),
                htmlOutput(outputId = "text")
              ) #end column with outcomes
            ), #end fluidrow containing input and output
            #Instructions section at bottom as tabs
            h2('Instructions'),
            #use external function to generate all tabs with instruction content
            withMathJax(do.call(tabsetPanel, generate_documentation(currentdocfilename)))
          ) #end tag list
        }) # End renderUI for analyze tab

      #once UI for the model in the analyze tab is created, switch to that tab
      updateNavbarPage(session, packagename, selected = "Analyze")
    }) #end observeEvent for the analyze tab

  }) #end lapply function surrounding observeEvent to build app

    #######################################################
    #end code that listens to model selection buttons and creates UI for a chosen model
    #######################################################
  
    ###############
    #Code to reset the model settings
    ###############
    observeEvent(input$reset, {
      modelinputs <- generate_shinyinput(mbmodel = appsettings$simfunction[1], otherinputs = appsettings$otherinputs, packagename = packagename)
      output$modelinputs <- renderUI({modelinputs})
      output$plotly <- NULL
      output$ggplot <- NULL
      output$text <- NULL
    })

    #######################################################
    #start code that listens to the 'run simulation' button and runs a model for the specified settings
    #######################################################
    observeEvent(input$submitBtn, {


      #run model with specified settings
      #run simulation, show a 'running simulation' message
      withProgress(message = 'Running Simulation',
                   detail = "This may take a while", value = 0,
                   {
                     #remove previous plots and text
                     output$ggplot <- NULL
                     output$plotly <- NULL
                     output$text <- NULL
                     #extract current model settings from UI input elements
                     x1=isolate(reactiveValuesToList(input)) #get all shiny inputs
                     x2 = x1[! (names(x1) %in% appNames)] #remove inputs that are action buttons for apps
                     x3 = (x2[! (names(x2) %in% c('submitBtn','Exit') ) ]) #remove further inputs
                     modelsettings = x3[!grepl("*selectized$", names(x3))] #remove any input with selectized
                     #remove nested list of shiny input tags
                     appsettings$otherinputs <- NULL
                     #add settings information from appsettings list
                     modelsettings = c(appsettings, modelsettings)
                     if (is.null(modelsettings$nreps)) {modelsettings$nreps <- 1} #if there is no UI input for replicates, assume reps is 1
                     #if no random seed is set in UI, set it to 123.
                     if (is.null(modelsettings$rngseed)) {modelsettings$rngseed <- 123}
                     #run model, process inside run_model function based on settings
                     result <- run_model(modelsettings)
                     #if things worked, result contains a list structure for processing with the plot and text functions  
                     #if things failed, result contains a string with an error message
                     if (is.character(result))
                     {
                       output$ggplot <- NULL
                       output$plotly <- NULL
                       output$text <- renderText({ paste("<font color=\"#FF0000\"><b>", result, "</b></font>") })
                     }
                     else #create plots and text, for plots, do either ggplot or plotly
                     {
                       if (modelsettings$plotengine == 'ggplot')
                       {
                         output$plotengine <- renderText('ggplot')
                         output$ggplot  <- shiny::renderPlot({ generate_ggplot(result) })
                       }
                       if (modelsettings$plotengine == 'plotly')
                       {
                         output$plotengine <- renderText('plotly')
                         output$plotly  <- plotly::renderPlotly({ generate_plotly(result) })
                       }
                       #create text from results
                       output$text <- renderText({ generate_text(result) })
                     }
                   }) #end with-progress wrapper
    } #end the expression being evaluated by observeevent
    ) #end observe-event for analyze model submit button

    #######################################################
    #end code that listens to the 'run simulation' button and runs a model for the specified settings
    #######################################################
  
  #######################################################
  #start code that listens to the "download code" button
  #######################################################
  
  output$download_code <- downloadHandler(
    filename = function() {
      "output.R"
    },
    content = function(file) {
      #extract current model settings from UI input elements
      x1=isolate(reactiveValuesToList(input)) #get all shiny inputs
      #x1=as.list( c(g = 1, U = 100)) #get all shiny inputs
      x2 = x1[! (names(x1) %in% appNames)] #remove inputs that are action buttons for apps
      x3 = (x2[! (names(x2) %in% c('submitBtn','Exit') ) ]) #remove further inputs
      modelsettings <- x3[!grepl("*selectized$", names(x3))] #remove any input with selectized
      modelsettings <- c(modelsettings, appsettings)
      modelfunction = modelsettings$simfunction
      if (is.null(modelsettings$nreps)) {modelsettings$nreps <- 1} #if there is no UI input for replicates, assume reps is 1
      #if no random seed is set in UI, set it to 123.
      if (is.null(modelsettings$rngseed)) {modelsettings$rngseed <- 123}
      
      output <- download_code(modelsettings, modelfunction)
      writeLines(output, file)
    }
  )
  
  #######################################################
  #end code that listens to the "download code" button
  #######################################################

  #######################################################
  #code that allows download of all files
  output$modeldownload <- downloadHandler(
    filename <- function() {
      "simulatorfunctions.zip"
    },
    content <- function(file) {
      file.copy(allsimfctfile, file)
    },
    contentType = "application/zip"
  )
  
  #######################################################
  #Button to create floating task list
  observeEvent(input$detachtasks, {
    x = withMathJax(generate_documentation(currentdocfilename))
    #browser()
    x1 = x[[2]][[3]] #task tab
    x2 = x1[[3]]
    x3 = x2[[1]][[3]] #pull out task list without buttons
    output$floattask <- renderUI({
      absolutePanel(x3, id = "taskfloat", class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                    width = "30%", height = "auto")
    })
  })
  
  #######################################################
  #Button to remove floating task list
  observeEvent(input$destroytasks, {
    output$floattask <- NULL
  })
  

  #######################################################
  #Exit main menu
  observeEvent(input$Exit, {
    stopApp('Exit')
  })

} #ends the server function for the app



#######################################################
#This is the UI for the Main Menu of DSAIDE
#######################################################

ui <- fluidPage(
  includeCSS("packagestyle.css"), #use custom styling
  tags$div(id = "shinyheadertitle", "DSAIDE - Dynamical Systems Approach to Infectious Disease Epidemiology"),
  tags$div(id = "shinyheadertext",
    "A collection of Shiny/R Apps to explore and simulate infectious disease models.",
    br()),
  tags$div(id = "infotext", paste0('This is ', packagename,  ' version ',utils::packageVersion(packagename),' last updated ', utils::packageDescription(packagename)$Date,'.')),
  tags$div(id = "infotext", "Written and maintained by", a("Andreas Handel", href="http://handelgroup.uga.edu", target="_blank"), "with contributions from", a("others.",  href="https://github.com/ahgroup/DSAIDE#contributors", target="_blank")),
  tags$div(id = "infotext", "More information can be found", a("on the package website.",  href="https://ahgroup.github.io/DSAIDE/", target="_blank")),
  navbarPage(title = packagename, id = packagename, selected = 'Menu',
             tabPanel(title = "Menu",
                      tags$div(class='mainsectionheader', 'The Basics'),
                      fluidRow(
                               actionButton("basicsir", "Basic SIR model", class="mainbutton"),
                               actionButton("idcharacteristics", "Characteristics of ID", class="mainbutton"),
                               actionButton("idpatterns", "ID Patterns", class="mainbutton"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'The Reproductive Number'),
                      fluidRow(
                        actionButton("reproductivenumber1", "Reproductive Number 1", class="mainbutton"),  
                        actionButton("reproductivenumber2", "Reproductive Number 2", class="mainbutton"),  
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'Controlling Infectious Diseases'),
                      fluidRow(
                        actionButton("idcontrolvaccine", "Basics of ID control", class="mainbutton"),  
                        actionButton("idcontrolmultioutbreak", "ID control for multiple outbreaks", class="mainbutton"),  
                        actionButton("idcontrolcomplex", "Complex ID control scenarios", class="mainbutton"), 
                        class = "mainmenurow"
                      ), #close fluidRow structure for input

                      tags$div(class='mainsectionheader', 'Types of transmission'),
                      fluidRow(
                        actionButton("directtransmission", "Direct transmission", class="mainbutton"),  
                      actionButton("environmentaltransmission", "Environmental transmission", class="mainbutton"),
                        actionButton("vectortransmission", "Vector transmission", class="mainbutton"),
                        class = "mainmenurow"
                      ), #close fluidRow structure for input
                      
                      tags$div(class='mainsectionheader', 'Stochastic models'),
                      fluidRow(
                        actionButton("stochasticsir", "Stochastic SIR model", class="mainbutton"),  
                         actionButton("stochasticseir", "Stochastic SEIR model", class="mainbutton"),
                         actionButton("evolutionarydynamics", "Evolutionary dynamics", class="mainbutton"),
                        class = "mainmenurow"
                      ),
                      tags$div(class='mainsectionheader', 'Further ID topics'),
                      fluidRow(
                        actionButton("hostheterogeneity", "Host heterogeneity", class="mainbutton"),  
                       actionButton("multipathogen", "Multi-Pathogen dynamics", class="mainbutton"),
                       actionButton("parasitemodel", "Parasite model", class="mainbutton"),
                       actionButton("idsurveillance", "ID surveillance", class="mainbutton"),
                       actionButton("maternalimmunity", "Maternal immunity", class="mainbutton"),
                       #actionButton("globalwarming", "Global Warming", class="mainbutton"),
                       #actionButton("metapopulation", "Metapopulation Dynamics", class="mainbutton"),
                       class = "mainmenurow"
                      ), #close fluidRow structure for input
                      tags$div(class='mainsectionheader', 'Fitting models to data'),
                      fluidRow(
                        actionButton("fitflu", "Fitting influenza data", class="mainbutton"),  
                        actionButton("fitnoro", "Fitting norovirus data", class="mainbutton"),  
                        class = "mainmenurow"
                      ), #close fluidRow structure for input
                      tags$div(class='mainsectionheader', 'Further modeling topics'),
                      fluidRow(
                        actionButton("modelexploration", "Model exploration", class="mainbutton"),  
                        actionButton("usanalysis", "Uncertainty and sensitivity analysis", class="mainbutton"),  
                        class = "mainmenurow"
                      ), #close fluidRow structure for input
                      withTags({
                        div(style = "text-align:left", class="bottomtext",
                            tags$div(id = "bottomtext", 'This collection of model simulations/apps provides covers various aspects of infectious disease epidemiology from a dynamical systems model perspective. The software is meant to provide you with a "learning by doing" approach. You will likely learn best and fastest by using this software as part of a course on the topic, taught by a knowledgable instructor who can provide any needed background information and help if you get stuck. Alternatively, you should be able to self-learn and obtain the needed background information by going through the materials listed in the "Further Information" section of the apps.'),
                            tags$div(id = "bottomtext", 'The main way of using the simulations is through this graphical interface. You can also access the simulations directly. This requires a bit of R coding but gives you many more options of things you can try. See the package vignette or the "Further Information" section of the apps for more on that.'),
                            tags$div(id = "bottomtext", 'You should start with the Basic SIR app and read all its instruction tabs since they contain information relevant for all apps. The remaining simulations are ordered in a sequence that makes sense for learning the material, so it is best to go in order (each section top to bottom, within each section left to right). Some simulations also build on earlier ones. But you can possibly also jump around and still be ok.')
                        )
                      }), #close withTags function
                      p('Have fun exploring the models!', class='maintext'),
                      fluidRow(
                        downloadButton("modeldownload", "Download R code for all simulations", class="mainbutton"),
                        actionButton("Exit", "Exit", class="exitbutton"),
                        class = "mainmenurow"
                      ) #close fluidRow structure for input

             ), #close "Menu" tabPanel tab

             tabPanel("Analyze",
                      fluidRow(
                        column(12,
                               uiOutput('analyzemodel'),
                               uiOutput('floattask')
                        )
                        #class = "mainmenurow"
                      ) #close fluidRow structure for input
             ) #close "Analyze" tab
  ), #close navbarPage

  tagList( hr(),
           p('All text and figures are licensed under a ',
           a("Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.", href="http://creativecommons.org/licenses/by-nc-sa/4.0/", target="_blank"),
           'Software/Code is licensed under ',
           a("GPL-3.", href="https://www.gnu.org/licenses/gpl-3.0.en.html" , target="_blank")
           ,
           br(),
           "The development of this package was partially supported by NIH grant U19AI117891.",
           align = "center", style="font-size:small") #end paragraph
  )
) #end fluidpage

shinyApp(ui = ui, server = server)