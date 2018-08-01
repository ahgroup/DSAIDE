#This is the Shiny App for the main menu

#this function is the server part of the app
server <- function(input, output, session) {

  appNames <- c(unlist(strsplit(DSAIDE::dsaideapps(),', ')),'Exit') #get list of all existing apps
  
  stopping <- FALSE
  
  lapply(appNames, function(appName) {
    observeEvent(input[[appName]], {
      stopping <<- TRUE
      stopApp(appName)
    })
  })
  
  session$onSessionEnded(function(){
    if (!stopping) {
      stopApp('Exit')
    }
  })
  
}



#This is the UI for the Main Menu of DSAIDE
ui <- fluidPage(
  includeCSS("../styles/dsaide.css"),
  #add header and title
  div( includeHTML("../styles/header.html"), align = "center"),
  p(paste('This is DSAIDE version ',utils::packageVersion("DSAIDE"),' last updated ', utils::packageDescription('DSAIDE')$Date,sep=''), class='infotext'),
  
  #specify name of App below, will show up in title
  h1('DSAIRM - Main Menu', align = "center", style = "background-color:#123c66; color:#fff"),

  p('The Basics', class='mainsectionheader'),

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
  
  
  p('The Reproductive Number', class='mainsectionheader'),
  fluidRow(
    column(6,
           actionButton("ReproductiveNumber1", "Reproductive Number I", class="mainbutton")  
    ),
    column(6,
           actionButton("ReproductiveNumber2", "Reproductive Number 2", class="mainbutton")  
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  
  p('Controlling Infectious Diseases', class='mainsectionheader'),
  fluidRow(
    column(6,
           actionButton("IDControl1", "ID Control 1", class="mainbutton")  
    ),
    column(6,
           actionButton("IDControl2", "ID Control 2", class="mainbutton")  
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  
  p('Types of Transmission', class='mainsectionheader'),
  
  
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
  
  
  p('Stochastic Models', class='mainsectionheader'),
  
  fluidRow(
    column(4,
           actionButton("StochasticSIR", "Stochastic SIR Model", class="mainbutton")  
    ),
    column(4,
           actionButton("StochasticSEIR", "Stochastic SEIR Model", class="mainbutton")  
    ),
    column(4,
           actionButton("EvolutionaryDynamics", "Evolutionary Dynamics", class="mainbutton")
    ),
    class = "mainmenurow"
  ),
  fluidRow(
    
    class = "mainmenurow"
  ),

  p('Further Topics', class='mainsectionheader'),
  fluidRow(
    column(6,
           actionButton("HostHeterogeneity", "Host Heterogeneity", class="mainbutton")  
    ),
    column(6, 
           actionButton("MultiPathogen", "Multi-Pathogen Dynamics", class="mainbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  
  fluidRow(
    
    column(12,
           actionButton("Exit", "Exit", class="exitbutton")
    ),
    class = "mainmenurow"
  ), #close fluidRow structure for input
  
  
  withTags({
    div(class="header", checked=NA, style = "text-align:left", class="infotext",
        
        p('This collection of Shiny apps provides you with a "learning by doing" way to explore various topics of infectious disease epidemiology from a dynamical systems perspective. Ideally, you would use these apps as part of a course on the topic. Alternatively, you should be able to obtain the needed background information by going through the materials listed in the "Further Information" section of the apps.'),
        p('The main way of using the simulations is through this graphical interface. You can also access the simulations directly. This requires a bit of R coding but gives you many more options of things you can try. See the package vignette or the "Further Information" section of the apps for more on that.'),
        p('You should start with the "ID Dynamics Intro" app and read all its instruction tabs since they contain information relevant for all apps.')
        
    )
  }),
  
  p('Have fun exploring the models!', class='maintext'),
  div(includeHTML("../styles/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

shinyApp(ui = ui, server = server)

  