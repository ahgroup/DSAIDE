#This is the Shiny server file for the ID Dynamics Introduction App

#the main function with all the functionality
#this function is wrapped inside the shiny server function below to allow return to main menu when window is closed

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


#This is the UI for the ID Dynamics Introduction App
ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  tags$head(
    tags$style(HTML("
                    img {
                    max-height: 90px;
                    max-width: '100%';
                    }
                    
                    body {
                    background-color: #fff;
                    }
                    "))
    ),
  
  div( includeHTML("www/header.html"), align = "center"),
  h1('ID Dynamics Introduction App', align = "center", style = "background-color:#123c66; color:#fff"),

  div( style="text-align:center", actionButton("IDDynamicsIntro", "Run ID Dynamics Intro")  ),
  div( style="text-align:center", actionButton("CharacteristicsofID", "Run Characteristics of ID")  ),
  div( style="text-align:center", actionButton("IDPatterns", "Run ID Patterns")  ),
  div( style="text-align:center", actionButton("ReproductiveNumber", "Run Reproductive Number")  ),
  div( style="text-align:center", actionButton("DirectTransmission", "Run Direct Transmission")  ),
  div( style="text-align:center", actionButton("EnvironmentalTransmission", "Run Environmental Transmission")  ),
  div( style="text-align:center", actionButton("VectorTransmission", "Run Vector Transmission")  ),
  div( style="text-align:center", actionButton("IDControl", "Run ID Control")  ),
  div( style="text-align:center", actionButton("HostHeterogeneity", "Run Host Heterogeneity")  ),
  div( style="text-align:center", actionButton("StochasticDynamics", "Run Stochastic Dynamics")  ),
  div( style="text-align:center", actionButton("EvolutionaryDynamics", "Run Evolutionary Dynamics")  ),
  div( style="text-align:center", actionButton("Exit", "Exit")  ),
  
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

shinyApp(ui = ui, server = server)
