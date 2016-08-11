#This is the UI for the ID Patterns App


ui <- fluidPage(
  includeCSS("../shinystyle.css"),

  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),

  div( includeHTML("www/header.html"), align = "center"),
  h1('ID Patterns App', align = "center", style = "background-color:#123c66; color:#fff"),


  div( style="text-align:center", actionButton("exitBtn", "Exit App") ),
  tags$hr(),


  # Inputs go at top above output
  # Each input uses either a slider or a text box except for the submit button
  fluidRow(
        column(4,
             sliderInput("PopSize", "Population Size", min = 1000, max = 5000, value = 1000, step = 500)
           ),
        column(4,
             sliderInput("P0", "Initial number of presymptomatic hosts", min = 0, max = 100, value = 0, step = 1)
            ),
        column(4,
           sliderInput("tmax", "Maximum simulation time (months)", min = 6, max = 12000, value = 120, step = 12)
          ),
        align = "center"
        ), #close fluidRow structure for input

  fluidRow(
          column(4,
              sliderInput("bP", "Level/Rate of transmission by presymptomatic hosts (bP, 1/months)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
              ),
          column(4,
              sliderInput("bA", "Level/Rate of transmission by asymptomatic hosts (bA, 1/months)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
              ),
          column(4,
                 sliderInput("bI", "Level/Rate of transmission by symptomatic hosts (bI, 1/months)", min = 0, max = 0.01, value = 0, step = 0.0001 , sep ='')
          ),
          align = "center"
          ), #close fluidRow structure for input
  fluidRow(
      column(4,
             sliderInput("gP", "Rate of recovery of presymptomatic hosts (gP, 1/months)", min = 0, max = 5, value = 0.5, step = 0.1)
      ),
      column(4,
             sliderInput("gA", "Rate of recovery of asymptomatic hosts (gA, 1/months)", min = 0, max = 5, value = 0.5, step = 0.1)
      ),
      column(4,
             sliderInput("gI", "Rate of recovery of symptomatic hosts (gI, 1/months)", min = 0, max = 5, value = 0.5, step = 0.1)
      ),
      align = "center"
  ), #close fluidRow structure for input
  fluidRow(
      column(4,
             sliderInput("f", "Fraction of asymptomatic infections (f)", min = 0, max = 1, value = 0, step = 0.1)
      ),
      column(4,
             sliderInput("d", "Fraction of deaths in symptomatic hosts (d)", min = 0, max = 1, value = 0, step = 0.1)
      ),
      column(4,
             sliderInput("w", "Rate of immunity loss (w, 1/months)", min = 0, max = 0.5, value = 0.0, step = 0.01 , sep ='')
      ),
      align = "center"
  ),
  fluidRow(
    column(4,
           sliderInput("lambda", "Monthly rate of new births (lambda)", min = 0, max = 100, value = 0, step = 1)
    ),
    column(4,
           sliderInput("n", "Natural death rate (n, 1/months)", min = 0, max = 0.02, value = 0, step = 0.0005)
    ),
    column(4,
           sliderInput("sigma", "Strength of seasonal variation of transmission (sigma)", min = 0, max = 1, value = 0, step = 0.1)
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

