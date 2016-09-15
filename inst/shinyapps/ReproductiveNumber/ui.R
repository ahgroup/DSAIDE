#This is the UI for the Reproductive Number App


ui <- fluidPage(
  includeCSS("../shinystyle.css"),

  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),

  div( includeHTML("www/header.html"), align = "center"),
  h1('Reproductive Number App', align = "center", style = "background-color:#123c66; color:#fff"),


  div( style="text-align:center", actionButton("exitBtn", "Exit App") ),
  tags$hr(),


  # Inputs go at top above output
  # Each input uses either a slider or a text box except for the submit button
  fluidRow(
    column(4,
           sliderInput("PopSize", "Initial Population Size", min = 1000, max = 5000, value = 1000, step = 500)
    ),
    column(4,
           sliderInput("I0", "Initial number of infected hosts", min = 0, max = 100, value = 0, step = 1)
    ),
    column(4,
           numericInput("tmax", "Maximum simulation time (months)", min = 0, max = 1200, value = 120)
    ),
    align = "center"
  ), #close fluidRow structure for input

  fluidRow(
    column(3,
           sliderInput("beta", "Rate of transmission (beta, 1/months)", min = 0, max = 0.1, value = 0, step = 0.001 , sep ='')
    ),
    column(3,
           sliderInput("gamma", "Rate at which a host leaves the infectious compartment (gamma, 1/months)", min = 0, max = 25, value = 10, step = 0.25, sep ='')
    ),
    column(3,
           sliderInput("f", "Fraction vaccinated prior to outbreak", min = 0, max = 1, value = 0, step = 0.05, sep ='')
    ),
    column(3,
           sliderInput("e", "Efficacy of vaccine", min = 0, max = 1, value = 0, step = 0.05, sep ='')
    ),
    align = "center"
  ), #close fluidRow structure for input
  fluidRow(
    column(4,
           sliderInput("lambda", "Monthly rate of new births (lambda)", min = 0, max = 100, value = 0, step = 1)
    ),
    column(4,
           sliderInput("n", "Natural death rate (n, 1/months)", min = 0, max = 0.02, value = 0, step = 0.0005, sep ='')
    ),
    column(4,
           sliderInput("w", "Rate of immunity loss (w, 1/months)", min = 0, max = 0.1, value = 0.0, step = 0.01)
    ),
    align = "center"
  ),

  div( style="text-align:center",          actionButton("submitBtn", "Run Simulation")  ),
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
  h2('Instructions'),

  #use external function to generate all tabs with instruction content

  do.call(tabsetPanel,generate_instruction_tabs()),

  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
) #end fluidpage

