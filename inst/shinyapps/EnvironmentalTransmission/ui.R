#This is the UI for the Environmental Transmission App

ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  
  div( includeHTML("www/header.html"), align = "center"),
  h1('Environmental Transmission App', align = "center", style = "background-color:#123c66; color:#fff"),
  
  #################################
  #end section to add buttons
  fluidRow(
    column(6,
           div( style="text-align:center", actionButton("submitBtn", "Run Simulation", style="color: #000000; background-color: #D2FFE2")  )
    ),
    column(6,
           div( style="text-align:center", actionButton("exitBtn", "Exit App", style="color: #000000; background-color: #BDCCD9") )
    )
    
  ), #end section to add buttons
  
  tags$hr(),

  
  #################################
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
                    sliderInput("I0", "initial number of infected hosts", min = 0, max = 50, value = 0, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("E0", "initial amount of environmental pathogen", min = 0, max = 100, value = 0, step = 1)
             ),
             column(6,
                    sliderInput("tmax", "Maximum simulation time (months)", min = 1, max = 500, value = 100, step = 1)
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("bd", "direct transmission rate (bd, 1/month)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             ),
             column(6,
                    sliderInput("be", "environmental transmission rate (be, 1/month)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(6,
                    sliderInput("p", "Rate of pathogen shedding by infected hosts (p, 1/month)", min = 0, max = 50, value = 1, step = 0.1)
             ),
             column(6,
                    sliderInput("c", "Rate of environmental pathogen decay (c, 1/month) ", min = 0, max = 10, value = 0, step = 0.01 , sep ='')
             )
           ), #close fluidRow structure for input
           fluidRow(
             column(4,
                    sliderInput("g", "Rate of recovery of infected hosts (g, 1/month)", min = 0, max = 2, value = 0.5, step = 0.1)
             ),
             column(4,
                    sliderInput("b", "Monthly rate of new births (b)", min = 0, max = 100, value = 0, step = 1)
             ),
             column(4,
                    sliderInput("n", "Natural death rate (n, 1/month)", min = 0, max = 0.02, value = 0, step = 0.0005, sep ='')
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
  #Ends the 2 column structure with inputs on left and outputs on right
  
  
  
  #################################
  #Instructions section at bottom as tabs
  h2('Instructions'),
  
  #use external function to generate all tabs with instruction content
  do.call(tabsetPanel,generate_instruction_tabs()),
  
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage


