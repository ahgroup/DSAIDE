#This is the UI for the Vector Transmission App

ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  
  div( includeHTML("www/header.html"), align = "center"),
  h1('Vector Transmission App', align = "center", style = "background-color:#123c66; color:#fff"),
  
  #################################
  #Start with results on top
  div( style="text-align:center", actionButton("exitBtn", "Exit App") ),
  tags$hr(),
  h2('Simulation Results'),
  plotOutput(outputId = "plot"),
  # PLaceholder for results of type text
  htmlOutput(outputId = "text"),
  #Placeholder for any possible warning or error messages (this will be shown in red)
  htmlOutput(outputId = "warn"),
  
  tags$head(tags$style("#warn{color: red;
                       font-style: italic;
                       }")),
  tags$hr(),
  div( style="text-align:center",          actionButton("submitBtn", "Run Simulation")  ),
  
  #################################
  # Inputs section
  h2('Simulation Settings'),
  fluidRow(
    column(3,
           sliderInput("Ph0", "Host Population Size", min = 1000, max = 5000, value = 1000, step = 500)
    ),
    column(3,
           sliderInput("Pv0", "Vector Population Size", min = 1000, max = 5000, value = 1000, step = 500)
    ),
    column(3,
           sliderInput("Ih0", "initial number of infected hosts", min = 0, max = 100, value = 0, step = 1)
    ),
    column(3,
           sliderInput("Iv0", "initial number of infected vectors", min = 0, max = 100, value = 0, step = 1)
    ),
    align = "center"
  ), #close fluidRow structure for input
  fluidRow(
    column(3,
           sliderInput("tmax", "Maximum simulation time (months)", min = 1, max = 500, value = 100, step = 1)
    ),
    column(3,
           sliderInput("b1", "vector to host transmission rate (b1, 1/month)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
    ),
    column(3,
           sliderInput("b2", "host to vector transmission rate (b2, 1/month)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
    ),
    column(3,
           sliderInput("w", "Rate of waning immunity (p, 1/month)", min = 0, max = 1, value = 0, step = 0.01, sep ='')
    ),
    align = "center"
  ), #close fluidRow structure for input
  fluidRow(
    column(3,
           sliderInput("g", "Rate of recovery of infected hosts (g, 1/month)", min = 0, max = 2, value = 0.5, step = 0.1)
    ),
    column(3,
           sliderInput("b", "Monthly rate of new vector births (b)", min = 0, max = 10000, value = 0, step = 100)
    ),
    column(3,
           sliderInput("n", "Natural vector death rate (n, 1/month)", min = 0, max = 2, value = 0, step = 0.01, sep ='')
    ),
    align = "center"
  ), #close fluidRow structure for input
  
  #################################
  #Instructions section
  h2('Instructions'),
  
  #use external function to generate all tabs with instruction content
  do.call(tabsetPanel,generate_instruction_tabs()),
  
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage


