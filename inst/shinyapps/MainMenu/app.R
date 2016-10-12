#This is the Shiny App for the main menu

#this function is the server part of the app
server <- function(input, output, session) {
  
  observeEvent(input$IDDynamicsIntro, {
    input$IDDynamicsIntro
    print ("Running IDDynamicsIntro...")
    stopApp(returnValue = 'A')
  })
  
  observeEvent(input$CharacteristicsofID, {
    input$CharacteristicsofID
    print ("Running CharacteristicsofID...")
    stopApp(returnValue = 'B')
  })
  
  observeEvent(input$IDPatterns, {
    input$IDPatterns
    print ("Running IDPatterns...")
    stopApp(returnValue = 'C')
  })
  
  observeEvent(input$ReproductiveNumber, {
    input$ReproductiveNumber
    print ("Running ReproductiveNumber...")
    stopApp(returnValue = 'D')
  })
  
  observeEvent(input$DirectTransmission, {
    input$DirectTransmission
    print ("Running DirectTransmission...")
    stopApp(returnValue = 'E')
  })
  
  observeEvent(input$EnvironmentalTransmission, {
    input$EnvironmentalTransmission
    print ("Running EnvironmentalTransmission...")
    stopApp(returnValue = 'F')
  })
  
  observeEvent(input$VectorTransmission, {
    input$VectorTransmission
    print ("Running VectorTransmission...")
    stopApp(returnValue = 'G')
  })
  
  observeEvent(input$IDControl, {
    input$IDControl
    print ("Running IDControl...")
    stopApp(returnValue = 'H')
  })
  
  observeEvent(input$HostHeterogeneity, {
    input$HostHeterogeneity
    print ("Running HostHeterogeneity...")
    stopApp(returnValue = 'I')
  })

  observeEvent(input$StochasticDynamics, {
    input$StochasticDynamics
    print ("Running StochasticDynamics...")
    stopApp(returnValue = 'J')
  })
  
  observeEvent(input$EvolutionaryDynamics, {
    input$EvolutionaryDynamics
    print ("Running EvolutionaryDynamics...")
    stopApp(returnValue = 'K')
  })

  
  observeEvent(input$Exit, {
    input$Exit
    print ("Exiting")
    stopApp(returnValue = 'X')
  })

  session$onSessionEnded(function(){
    stopApp(returnValue = 'X')
  })
  
}


#This is the UI for the Main Menu of DSAIDE
ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),

  h1('DSAIDE - Main Menu', align = "center", style = "background-color:#123c66; color:#fff"),

  p(paste('This is DSAIDE version ',utils::packageVersion("DSAIDE"),' last updated ', utils::packageDescription('DSAIDE')$Date,sep=''), class='maintext'),
  p('Have fun exploring the infectious disease models!', class='maintext'),
  p('Note that not all Apps are available yet', class='maintext'),
  
  fluidRow(
    column(4,
           actionButton("IDDynamicsIntro", "ID Dynamics Intro", class="mainbutton")
    ),
    column(4,
           actionButton("CharacteristicsofID", "Characteristics of ID", class="mainbutton")
    ),
    column(4,
           actionButton("IDPatterns", "ID Patterns", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  fluidRow(
    column(4,
           actionButton("DirectTransmission", "Direct Transmission", class="mainbutton")  
    ),
    column(4,
         actionButton("EnvironmentalTransmission", "Environmental Transmission", class="mainbutton")  
    ),
    column(4,
           actionButton("VectorTransmission", "Vector Transmission", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  fluidRow(
    column(4,
           actionButton("ReproductiveNumber", "Reproductive Number", class="mainbutton")  
    ),
    column(4,
           actionButton("IDControl", "ID Control", class="mainbutton")  
    ),
    column(4,
           actionButton("HostHeterogeneity", "Host Heterogeneity", class="mainbutton")  
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  fluidRow(
    column(4,
           actionButton("StochasticDynamics", "Stochastic Dynamics", class="mainbutton")  
    ),
    column(4,
           actionButton("EvolutionaryDynamics", "Evolutionary Dynamics", class="mainbutton")
    ),
    column(4, 
            ""
    ),
    class = "mainmenurow"
  ),
  fluidRow(
      
    column(12,
           actionButton("Exit", "Exit", class="exitbutton")
    ),
    class = "mainmenurow"
    ), #close fluidRow structure for input
  
  
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

shinyApp(ui = ui, server = server)
