#This is the UI for the Reproductive Number App

ui <- fluidPage(
  includeCSS("../shinystyle.css"),

  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  h1('Reproductive Number App', align = "center", style = "background-color:#123c66; color:#fff"),
  #h1('Vaccine Simulation App', align = "center", style = "background-color:#123c66; color:#fff"),
  
  #start section to add buttons
  fluidRow(
    column(6,
           div( style="text-align:center", actionButton("submitBtn", "Run Simulation", style="color: #000000; background-color: #D2FFE2")  )
    ),
    column(6,
           div( style="text-align:center", actionButton("exitBtn", "Exit App", style="color: #000000; background-color: #BDCCD9") )
    )
    
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
                    sliderInput("PopSize", "Population Size", min = 1000, max = 5000, value = 1000, step = 500)
             ),
             column(6,
                    sliderInput("I0", "initial number of infected hosts", min = 0, max = 100, value = 0, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("tmax", "Maximum simulation time (months)", min = 1, max = 1200, value = 100, step = 1)
             ),
             column(6,
                    sliderInput("w", "Rate of immunity loss (w, 1/months)", min = 0, max = 10, value = 0, step = 0.01)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("beta", "Rate of transmission (beta, 1/months)", min = 0, max = 0.1, value = 0, step = 0.001 , sep ='')
             ),
             column(6,
                    sliderInput("gamma", "Rate at which a host leaves the infectious compartment (gamma, 1/months)", min = 0, max = 25, value = 10, step = 0.25, sep ='')
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("f", "Fraction vaccinated prior to outbreak", min = 0, max = 1, value = 0, step = 0.05, sep ='')
             ),
             column(6,
                    sliderInput("e", "Efficacy of vaccine", min = 0, max = 1, value = 0, step = 0.05, sep ='')
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("lambda", "Monthly rate of new births (lambda)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(6,
                    sliderInput("n", "Natural death rate (n, 1/months)", min = 0, max = 0.02, value = 0, step = 0.0005, sep ='')
             )
           ) #close fluidRow structure for input
           
    ), #end sidebar column for inputs
    
    #all the outcomes here
    column(6,
           
           #################################
           #Start with results on top
           h2('Simulation Results'),
           plotOutput(outputId = "plot"),
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

