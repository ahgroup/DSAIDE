#This is the UI for the ID Control App


ui <- fluidPage(
  
  includeCSS("../shinystyle.css"),
  
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  h1('ID Control App', align = "center", style = "background-color:#123c66; color:#fff"),

  #start section to add buttons
  fluidRow(
    column(6,
           div( style="text-align:center", actionButton("submitBtn", "Run Simulation", style="color: #000000; background-color: #D2FFE2")  )
    ),
    column(6,
           actionButton("exitBtn", "Exit App", class="exitbutton")
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
             column(4,
                    sliderInput("S0", "initial number of susceptible hosts", min = 100, max = 5000, value = 1000, step = 100)
             ),
             column(4,
                    sliderInput("I0", "initial number of symptomatic hosts", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                      sliderInput("E0", "initial amount of environmental pathogen", min = 0, max = 5000, value = 0, step = 100)
             )
             ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("Sv0", "initial number of susceptible vectors", min = 0, max = 5000, value = 0, step = 100)
             ),
             column(4,
                    sliderInput("Iv0", "initial number of infected vectors", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    sliderInput("tmax", "Maximum simulation time (months)", min = 1, max = 1200, value = 100, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("bP", "Rate of transmission from pre-symptomatic hosts", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(4,
                    sliderInput("bA", "Rate of transmission from asymptomatic hosts", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(4,
                                    sliderInput("bI", "Rate of transmission from symptomatic hosts", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             )
           ), #close fluidRow structure for input
          fluidRow(
               column(4,
                      sliderInput("bE", "Rate of transmission from environment", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
               ),
               column(4,
                      sliderInput("bv", "Rate of transmission from vectors", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
               ),
               column(4,
                      sliderInput("bh", "Rate of transmission to vectors", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
               )
             ), #close fluidRow structure for input
             fluidRow(
               column(4,
                      sliderInput("gP", "Rate at which presymptomatic hosts leave compartment", min = 0, max = 5, value = 0.5, step = 0.1)
               ),
               column(4,
                      sliderInput("gA", "Rate at which asymptomatic hosts leave compartment", min = 0, max = 5, value = 0.5, step = 0.1)
               ),
               column(4,
                      sliderInput("gI", "Rate at which symptomatic hosts leave compartment", min = 0, max = 5, value = 0.5, step = 0.1)
               )
             ), #close fluidRow structure for input
          
             fluidRow(
                 column(4,
                        sliderInput("pA", "Rate of pathogen shedding by asymptomatic hosts", min = 0, max = 10, value = 0, step = 0.1)
                 ),
                 column(4,
                        sliderInput("pI", "Rate of pathogen shedding by symptomatic hosts", min = 0, max = 10, value = 0, step = 0.1)
                 ),
                 column(4,
                        sliderInput("c", "Rate of environmental pathogen decay", min = 0, max = 10, value = 0, step = 0.1 , sep ='')
                 )
            ), #close fluidRow structure for input
            fluidRow(
                 column(4,
                        sliderInput("f", "Fraction of hosts that are asymptomatic", min = 0, max = 1, value = 0, step = 0.1)
                 ),
                 column(4,
                        sliderInput("w", "Rate of waning immunity (w, 1/month)", min = 0, max = 50, value = 0, step = 0.1)
                 ),
                 column(4,
                        sliderInput("d", "Fraction of symptomatic hosts that die", min = 0, max = 1, value = 0, step = 0.01 , sep ='')
                 )
             ), #close fluidRow structure for input
             fluidRow(
                 column(6,
                        sliderInput("birthh", "birth rate of hosts", min = 0, max = 100, value = 0, step = 0.01 , sep ='')
                 ),
                 column(6,
                        sliderInput("deathh", "death rate of hosts", min = 0, max = 100, value = 0, step = 0.01 , sep ='')
                 )
             ), #close fluidRow structure for input
             fluidRow(
                 column(6,
                        sliderInput("birthv", "birth rate of vectors", min = 0, max = 5000, value = 0, step = 1 , sep ='')
                 ),
                 column(6,
                        sliderInput("deathv", "death rate of vectors", min = 0, max = 30, value = 0, step = 0.1 , sep ='')
                 )
            ) #close fluidRow structure for input
               
             
    ), #end sidebar column for inputs
    
    #all the outcomes here
    column(6,
           
           #################################
           #Start with results on top
           h2('Simulation Results'),
           plotOutput(outputId = "plot", height = "800px"),
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