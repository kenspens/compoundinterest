library(ggplot2)
db <- dbConnect(RSQLite::SQLite(), dbname = "/Users/kendallspencer/Downloads/f1wheels.db")
compound_type <- dbGetQuery(db, 'SELECT distinct compound from stints')
compound_type
stints <- dbGetQuery(db, 'SELECT * from stints')
drivers <- dbGetQuery(db, 'SELECT * from drivers')
sessions <- dbGetQuery(db, 'SELECT * from sessions')
results <- dbGetQuery(db, 'SELECT * from results')

datavizUI <- function(id) {
  ns <- NS(id) # Namespace function to isolate element IDs
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = ns("selected_driver"), # Wrapped in ns()
          label = "Select Driver Number:",
          choices = NULL,
          selected = NULL
        ),
        hr(),
        helpText("Select a driver to update the race positions and stint analysis charts.")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Driver Positions", plotOutput(ns("positionPlot"))),   # Wrapped in ns()
          tabPanel("Session Appearances", plotOutput(ns("sessionPlot"))), # Wrapped in ns()
          tabPanel("Tyre Strategy", plotOutput(ns("stintPlot")))          # Wrapped in ns()
        )
      )
    )
  )
}

datavizServer <- function(id, results_data, stints_data) {
  moduleServer(id, function(input, output, session) {
    
    # Populate driver choices dynamically at startup based on passed data
    observe({
      # Evaluate the data from the passed reactives
      df_results <- results_data()
      df_stints <- stints_data()
      
      all_drivers <- unique(c(as.character(df_results$driver), as.character(df_stints$driver)))
      all_drivers <- sort(all_drivers)
      
      # Note: Do NOT use ns() inside updateSelectInput target id
      updateSelectInput(session, "selected_driver",
                        choices = all_drivers,
                        selected = if("81" %in% all_drivers) "81" else all_drivers[1])
    })
    
    # Chart 1: Driver Position History (Line Chart)
    output$positionPlot <- renderPlot({
      req(input$selected_driver)
      
      results_data() %>%
        filter(driver == input$selected_driver) %>%
        ggplot(aes(x = result_session, y = as.numeric(position), color = driver, group = driver)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_y_reverse(breaks = 1:20) +
        labs(
          title = paste("Driver", input$selected_driver, "Position History"),
          x = "Result Session",
          y = "Finishing Position"
        ) +
        theme_minimal()
    })
    
    # Chart 2: Race Session Counts (Bar Chart)
    output$sessionPlot <- renderPlot({
      req(input$selected_driver)
      
      stints_data() %>%
        filter(as.character(driver) == as.character(input$selected_driver)) %>%
        ggplot(aes(x = race_session)) +
        geom_bar(fill = "steelblue", color = "white") +
        labs(
          title = paste("Driver", input$selected_driver, "Session Appearances"),
          x = "Race Session",
          y = "Count"
        ) +
        theme_minimal()
    })
    
    # Chart 3: Stint Number & Tyre Compounds (Stacked Bar Chart)
    output$stintPlot <- renderPlot({
      req(input$selected_driver)
      
      stints_data() %>%
        filter(as.character(driver) == as.character(input$selected_driver)) %>%
        ggplot(aes(x = stint_number, fill = compound)) +
        geom_bar(position = "stack") +
        labs(
          title = paste("Driver", input$selected_driver, "Tyre Compound Strategy by Stint"),
          x = "Stint Number",
          y = "Frequency / Count",
          fill = "Tyre Compound"
        ) +
        theme_minimal()
    })
  })
}