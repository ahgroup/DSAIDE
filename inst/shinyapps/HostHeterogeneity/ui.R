#This is the UI for the Host Heterogeneity App


ui <- fluidPage(
  
  includeCSS("../shinystyle.css"),
  
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  h1('Host Heterogeneity App', align = "center", style = "background-color:#123c66; color:#fff"),

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
           p('All parameters are assumed to be in units of (inverse) months'),
           fluidRow(
             column(6,
                    sliderInput("S10", "initial number of susceptible type 1 hosts", min = 100, max = 5000, value = 1000, step = 100)
             ),
             column(6,
                    sliderInput("I10", "initial number of infected type 1  hosts", min = 0, max = 100, value = 0, step = 1)
             )
             ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("S20", "initial  number of susceptible type 2 hosts", min = 0, max = 5000, value = 0, step = 100)
             ),
             column(6,
                    sliderInput("I20", "initial  number of infected type 2 hosts", min = 0, max = 100, value = 0, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("b11", "Rate of transmission between type 1  hosts", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(6,
                    sliderInput("b22", "Rate of transmission between type 2 hosts", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             )
           ), #close fluidRow structure for input
          fluidRow(
               column(6,
                      sliderInput("b12", "Rate of transmission from type 2 to type 1", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
               ),
               column(6,
                      sliderInput("b21", "Rate of transmission from type 1 to type 2", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
               )
             ), #close fluidRow structure for input
             fluidRow(
               column(6,
                      sliderInput("g1", "Rate at which infected type 1 hosts leave compartment", min = 0, max = 5, value = 0.5, step = 0.1)
               ),
               column(6,
                      sliderInput("g2", "Rate at which infected type 2 hosts leave compartment", min = 0, max = 5, value = 0.5, step = 0.1)
               )
             ), #close fluidRow structure for input
          
             
            fluidRow(
                 column(4,
                        sliderInput("w1", "Rate of waning immunity of type 1", min = 0, max = 1, value = 0, step = 0.1)
                 ),
                 column(4,
                        sliderInput("w2", "Rate of waning immunity of type 2", min = 0, max = 50, value = 0, step = 0.1)
                 ),
                 column(4,
                        sliderInput("tmax", "Maximum simulation time (months)", min = 1, max = 1200, value = 100, step = 1)
                 )
                 
             ), #close fluidRow structure for input
             
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