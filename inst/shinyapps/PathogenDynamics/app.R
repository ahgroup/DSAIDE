# Outline of app:
# The user can select the probability of infection in a given period by each pathogen,
# the probability of recovery in a given period from each infection, and the maximum
# possible number of time periods. The server will then run a while loop where the
# host is exposed to whatever pathogens to which it is vulnerable in each period.
# The function records which pathogens, if any, infected the host, and how long
# the host harbored the pathogen(s). The loop breaks when either the available
# time periods run out, or when the host has become immune to both pathogens.
# The server will then return 1) length of the simulation time, 2) which pathogen(s),
# if any, infected the host, and 3) if the host was infected, how long it was
# infected with each pathogen. This output will then be printed as a data table
# in the main panel of the user interface.

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("../shinystyle.css"),
  #add header and title
  tags$head( tags$script(src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML", type = 'text/javascript') ),
  tags$head(tags$style(".myrow{vertical-align: bottom;}")),
  div( includeHTML("www/header.html"), align = "center"),
  #specify name of App below, will show up in title
  h1('Pathogen Dynamics App', align = "center", style = "background-color:#123c66; color:#fff"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select the parameters for the pathogen simulation."),
      numericInput(inputId = "pathA_inf", label = "Probability of Infection by Pathogen A",
                   value = 0.2, min = 0, max = 1, step = 0.01),
      numericInput(inputId = "pathB_inf", label = "Probability of Infection by Pathogen B",
                   value = 0.2, min = 0, max = 1, step = 0.01),
      numericInput(inputId = "pathA_rec", label = "Probability of Recovery from Pathogen A",
                   value = 0.2, min = 0, max = 1, step = 0.01),
      numericInput(inputId = "pathB_rec", label = "Probability of Recovery from Pathogen B",
                   value = 0.2, min = 0, max = 1, step = 0.01),
      numericInput(inputId = "sim_length", label = "Length of Simulation Time",
                   value = 10, min = 5, max = 100, step = 1)
    ),
    mainPanel(
    #  verbatimTextOutput("sim_results")
      tableOutput("sim_results")
    )
  ),
  
  #################################
  #Instructions section at bottom as tabs
  h2('Instructions'),
  #use external function to generate all tabs with instruction content
  do.call(tabsetPanel,generate_instruction_tabs()),
  div(includeHTML("www/footer.html"), align="center", style="font-size:small") #footer

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$sim_results <- renderTable({
    time_periods <- input$sim_length
    mat_form <- matrix(rep(0, time_periods*5), ncol = 5)
    results_df <- as.data.frame(mat_form)
  #  names(results_df) <- c("Time Point", "Infected With A?", "Infected With B?", "Time Infected With A", 
  #                         "Time Infected With B")
    names(results_df) <- c("A", "B", "C", "D", "E")
    exp_A <- FALSE
    exp_B <- FALSE
    time_A <- 0
    time_B <- 0
    time_point <- 1
    while (time_point <= time_periods) {
      if (exp_A == TRUE | exp_A == FALSE) {
        exp_A <- ifelse(exp_A == TRUE, sample(x = c(TRUE, "Finished"), size = 1,
                                              prob = c(1 - input$pathA_rec, input$pathA_rec)),
                        sample(x = c(TRUE, FALSE), size = 1, prob = c(input$pathA_inf, 1 - input$pathA_inf)))
      }
      
      if (exp_B == TRUE | exp_B == FALSE) {
        exp_B <- ifelse(exp_B == TRUE, sample(x = c(TRUE, "Finished"), size = 1,
                                              prob = c(1 - input$pathB_rec, input$pathB_rec)),
                        sample(x = c(TRUE, FALSE), size = 1, prob = c(input$pathB_inf, 1 - input$pathB_inf)))
      }
      
      time_A <- ifelse(exp_A == TRUE, time_A + 1, time_A)
      time_B <- ifelse(exp_B == TRUE, time_B + 1, time_B)
      
      results_df[time_point, ] <- c(time_point, exp_A, exp_B, time_A, time_B)
      
      time_point <- time_point + 1
      
      
    }

    return(results_df)
    
  })
  
  # output$results_table <- renderDataTable({
  #   sim_results()
  # })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

