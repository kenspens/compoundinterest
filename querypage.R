library(DBI)
library(RSQLite)
library(shiny)
library(shinydashboard)
library(dbplyr)
library(dplyr)
library(pool)
library(DT)
library(purrr)
library(data.table)

db <- dbConnect(RSQLite::SQLite(), dbname = "/Users/kendallspencer/Downloads/f1wheels.db")
dbListTables(db)
sqlConsoleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             titlePanel("SQL Query Console"),
             textAreaInput(
               ns("querying"), 
               "Query the database in SQL:", 
               placeholder = "SELECT * FROM your_table_name LIMIT 10;", 
               rows = 5, 
               width = "100%"
             ),
             actionButton(
               ns("run_query"), 
               "Run Query", 
               class = "btn-success", 
               icon = icon("play")
             ),
             br(), br(),
             uiOutput(ns("error_message")),
             # Data Output
             DT::dataTableOutput(ns("myquery"))
      )
    )
  )
}

sqlConsoleServer <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    error_state <- reactiveVal(NULL)
    query_results <- eventReactive(input$run_query, {
      req(input$querying) # Stop if field is empty
      
      error_state(NULL)
      
      result <- tryCatch({
        dbGetQuery(conn = db, statement = input$querying)
      }, error = function(e) {
        error_state(e$message) # Log the database engine failure details
        return(NULL)
      })
      
      return(result)
    })
    
    output$error_message <- renderUI({
      req(error_state())
      div(
        class = "alert alert-danger", 
        role = "alert",
        tags$b("SQL Error: "), error_state()
      )
    })
    
    output$myquery <- DT::renderDataTable({
      req(query_results())
      query_results()
    }, options = list(scrollX = TRUE, pageLength = 10))
    
  })
}

