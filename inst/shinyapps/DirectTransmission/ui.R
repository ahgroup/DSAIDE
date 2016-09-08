#This is the UI for the Direct Transmission App

ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  
  div( includeHTML("www/header.html"), align = "center"),
  h1('Modes of Direct Transmission App', align = "center", style = "background-color:#123c66; color:#fff"),
  
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
    column(4,
           sliderInput("PopSize", "Population Size", min = 1000, max = 5000, value = 1000, step = 500)
    ),
    column(4,
           sliderInput("I0", "initial number of infected hosts", min = 0, max = 100, value = 0, step = 1)
    ),
    column(4,
           sliderInput("tmax", "Maximum simulation time (months)", min = 1, max = 500, value = 100, step = 1)
    ),
    align = "center"
  ), #close fluidRow structure for input
  fluidRow(
    column(3,
           selectInput("scenario", "Transmission scenario:",c("density dependent" = 1, 'frequency dependent' = 2))
    ),
    column(3,
           sliderInput("bd", "density dependent transmission rate (bd, 1/months)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
    ),
    column(3,
           sliderInput("bf", "frequency dependent transmission rate (bf, 1/months)", min = 0, max = 10, value = 0, step = 0.1 , sep ='')
    ),
    column(3,
           sliderInput("A", "Area where hosts interact (for density dependent transmission, arbitrary units)", min = 0.1, max = 10, value = 1, step = 0.1)
    ),
    align = "center"
  ), #close fluidRow structure for input
  fluidRow(
    column(3,
           sliderInput("g", "Rate of recovery of infected hosts (g, 1/months)", min = 0, max = 2, value = 0.5, step = 0.1)
    ),
    column(3,
           sliderInput("w", "Rate of immunity loss (w, 1/months)", min = 0, max = 0.1, value = 0.0, step = 0.01 , sep ='')
    ),
    column(3,
           sliderInput("b", "Monthly rate of new births (b)", min = 0, max = 100, value = 0, step = 1)
    ),
    column(3,
           sliderInput("n", "Natural death rate (n, 1/months)", min = 0, max = 0.02, value = 0, step = 0.0005, sep ='')
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


