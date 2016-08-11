#This is the UI for the ID Dynamics Introduction App


ui <- fluidPage(
  includeCSS("../shinystyle.css"),

  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),

  div( includeHTML("www/header.html"), align = "center"),
  h1('ID Dynamics Introduction App', align = "center", style = "background-color:#123c66; color:#fff"),


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
           sliderInput("tmax", "Maximum simulation time (days)", min = 10, max = 1000, value = 300, step = 10)
          ),
        align = "center"
        ), #close fluidRow structure for input

  fluidRow(
          column(6,
              sliderInput("beta", "Rate of transmission (beta, 1/days)", min = 0, max = 0.01, value = 0, step = 0.0001, sep ='')
              ),
          column(6,
              sliderInput("gamma", "Rate at which a host leaves the infectious compartment (gamma, 1/days)", min = 0, max = 2, value = 0.5, step = 0.1)
              ),
          align = "center"
          ), #close fluidRow structure for input

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

