## Simple Shiny app for fitting linear models.

# refresh <- function(input, output) {
#   res <- reactive({
#     input$submitBtn
#     
#     b0 <- isolate(input$b0);
#     b1 <- isolate(input$b1);
#     n <- isolate(input$n);
#     sigma <- isolate(input$sigma);
#     
#     output$plot <- renderPlot({
#       generate_linear_data(b0 = b0, b1 = b1, sigma = sigma, n = n)
#     })
#   })
# }

refresh <- function(input, output) {
  input$submitBtn
  
  b0 <- isolate(input$b0);
  b1 <- isolate(input$b1);
  n <- isolate(input$n);
  sigma <- isolate(input$sigma);
  
  output$plot <- renderPlot({generate_linear_data(b0 = b0, b1 = b1, sigma = sigma, n = n)})
  
}

server <- function(input, output, session) {
  # Waits for the Exit Button to be pressed to stop the app and return to main menu
  observeEvent(input$exitBtn, {
    input$exitBtn
    stopApp(returnValue = 0)
  })
  
  refresh(input, output)
  
  
  session$onSessionEnded(function(){
    stopApp(returnValue = 0)
  })
}



ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  div( includeHTML("www/header.html"), align = "center"),
  #specify name of App below, will show up in title
  h1('Host Heterogeneity App', align = "center", style = "background-color:#123c66; color:#fff"),
  
  #section to add buttons
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
  
  ## Split screen with input on left, output on right.
  fluidRow(
    column(6,
           # Inputs
           h2("Enter the intercept, slope, sample size, and standard deviation of the error term."),
           numericInput(inputId = "b0", label = "Intercept", value = 10),
           numericInput(inputId = "b1", label = "Slope", value = 10),
           numericInput(inputId = "n", label = "n", value = 150),
           numericInput(inputId = "sigma", label = "Sigma", value = 2)
           ),
    column(6,
           # Outputs
           h2("Plot of Points with Model Line"),
           plotOutput(outputId = "plot", height = "500px"),
           # PLaceholder for results of type text
           htmlOutput(outputId = "text"),
           #Placeholder for any possible warning or error messages (this will be shown in red)
           htmlOutput(outputId = "warn"),
           
           tags$head(tags$style("#warn{color: red;
                                font-style: italic;
                                }")),
           tags$hr()
           
           ),
    #Instructions section at bottom as tabs
    h2('Instructions'),
    #use external function to generate all tabs with instruction content
    do.call(tabsetPanel,generate_instruction_tabs()),
    div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer
  )
)

shinyApp(ui = ui, server = server)
  
  
  

  
  
  
  
  