#this is the UI for the
#The UI loads the sliders, output and instructions (the latter as separate html files)
shinyUI(fluidPage(
  includeHTML("www/header.html"),
  br(),
  div( style="text-align:center", actionButton("exitBtn", "Exit App") ), #the button which can be used to exit the App
  br(),
  sidebarLayout(

    # Side panel - contains the inputs, i.e. sliders/boxes to set parameters and other stuff
    sidebarPanel(
      sliderInput("PopSize", "Initial Population Size", min = 1e5, max = 1e6, value = 5e5),
      sliderInput("I0", "Initial number of infected hosts", min = 1, max = 100, value = 1),
      sliderInput("R0", "Initial number of recovered hosts", min = 0, max = 100, value = 0),
      sliderInput("tmax", "Maximum simulation time (years)", min = 1, max = 20, value = 5),
      sliderInput("gamma", "Rate (1/year) at which a host leaves the infectious compartment", min = 0, max = 25, value = 13),
      textInput("betaD", "Rate of transmission of pathogen from infected to susceptible host (density)", value = "4e-5"),
      sliderInput("betaF", "Rate of transmission of pathogen from infected to susceptible host (frequency)", min = 1, max = 100, value = 40),
      sliderInput("mu", "Death rate (1/year)", min = 0, max = 1, value =  1 / 50),
      sliderInput("w", "Duration of immunity (years)", min = 0, max = 20, value = 0),
      textInput("k", "Constant of proportionality for population-size dependent area", value = "1e-6"),
      sliderInput("scenario", "Choose transmission scenario", min = 1, max = 3, value = 1, step = 1),
      actionButton("submitBtn", "Run Simulation")
      ), # End of Sidebar Input Panel

    # Main Panel - contains the model output on top and the tabs with information/instruction below
    mainPanel(
      # Tab Set Panels (i.e. sub-panels) included in main panel
      tabsetPanel(
        # Output tab to display all the output values and plots
        tabPanel(
          "Output",
          plotOutput("plot", width = "200%"),
          htmlOutput(outputId = "text"),
          htmlOutput(outputId = "warn"),
          tags$head(tags$style("#warn{color: red;
                               font-style: italic;
                               }"))
        )
      ), # End of tab set that displays the simulation output
      br(),
      do.call(tabsetPanel,generate_instruction_tabs()) #use external function to generate all tabs with instruction content

    ) # End of Main Panel containing output and instruction tabs

  ), # End of sidebar layout (i.e. side panel and main panel)

  # Footer Row
  fluidRow(
    includeHTML("www/footer.html")
  ) # End of Footer
) #ends fluid-page layout
) #ends 'everything', i.e. the Shiny UI function
