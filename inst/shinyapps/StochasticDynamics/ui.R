#This is the UI for the Stochastic Dynamics App


ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  h1('Host Heterogeneity App', align = "center", style = "background-color:#123c66; color:#fff"),
  
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
           p('All parameters are assumed to be in units of (inverse) months'),
           fluidRow(
             column(4,
                    sliderInput("S0", "initial number of susceptible hosts", min = 100, max = 5000, value = 1000, step = 100)
             ),
             column(4,
                    sliderInput("I0", "initial number of symptomatic hosts", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    sliderInput("tmax", "Maximum simulation time (months)", min = 1, max = 1200, value = 100, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("bP", "Rate of transmission of presymptomatic hosts", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(4,
                    sliderInput("bI", "Rate of transmission of symptomatic hosts", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(4,
                    sliderInput("w", "Rate of immunity loss", min = 0, max = 0.1, value = 0.0, step = 0.01)
             )
           ), #close fluidRow structure for input
           
           fluidRow(
             column(4,
                    sliderInput("gP", "Rate at which presymptomatic hosts leave compartment", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(4,
                    sliderInput("gI", "Rate at which symptomatic hosts leave compartment", min = 0, max = 5, value = 0.5, step = 0.1)
             ),
             column(4,
                    sliderInput("nreps", "Number of simulations", min = 1, max = 50, value = 1, step = 1)
             )
           ), #close fluidRow structure for input
  fluidRow(
    column(4,
           sliderInput("lambda", "Rate of new births", min = 0, max = 10000, value = 0, step = 100)
    ),
    column(4,
           sliderInput("n", "Natural death rate", min = 0, max = 1, value = 0, step = 0.1)
    ),
    column(4,
           sliderInput("sigma", "Strength of seasonal variation of transmission", min = 0, max = 1, value = 0, step = 0.1)
    ),
    align = "center"
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
  