#WE PUTTING IT ALL TOGETHER
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
metables <- dbListTables(db)

library(shinydashboard)
source("/Users/kendallspencer/tire_app/MARKWATNEYSPACEPIRATE/dataviztab.R", local = TRUE)
source("/Users/kendallspencer/tire_app/MARKWATNEYSPACEPIRATE/crudmodule.R", local = TRUE)
source("/Users/kendallspencer/tire_app/MARKWATNEYSPACEPIRATE/querypage.R", local = TRUE)
library(shinythemes)
# ==========================================
# MAIN APPLICATION UI
# ==========================================
ui <- fluidPage(theme = shinytheme("united"),
  titlePanel("Compound Interest"),
  p("An F1 Tire Information Database App."),
  hr(),
  
  # Tabbed navigation hosting three unique module namespaces
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Read the Data", 
             # Fix 1: Passed required 'metables' vector matching your database tables
             dbCrudUI(id = "tab_1", metables = c("drivers","stints","sessions","results"))
    ),
    tabPanel("Querying", 
             sqlConsoleUI(id = "tab_2")
    ),
    tabPanel("Data Visualization", 
             datavizUI(id = "tab_3")
    )
  )
)

# ==========================================
# MAIN APPLICATION SERVER
# ==========================================
server <- function(input, output, session) {
  
  # Establish local database connection string path
  db <- dbConnect(RSQLite::SQLite(), dbname = "/Users/kendallspencer/Downloads/f1wheels.db")
  
  # Ensure the connection closes automatically when the app stops running
  onStop(function() {
    dbDisconnect(db)
  })
  
  # Fetch data metrics immediately on startup
  stints_df   <- dbGetQuery(db, 'SELECT * from stints')
  drivers_df  <- dbGetQuery(db, 'SELECT * from drivers')
  sessions_df <- dbGetQuery(db, 'SELECT * from sessions')
  results_df  <- dbGetQuery(db, 'SELECT * from results')
  
  # Fix 2: Wrap datasets in reactives so datavizServer can accept them safely
  reactive_results <- reactive({ results_df })
  reactive_stints  <- reactive({ stints_df })
  
  # Fix 3: Standardize IDs so Server blocks match UI blocks exactly
  
  # 1. Call CRUD Module (Matches UI ID: "tab_1")
  dbCrudServer(
    id = "tab_1", 
    db = db
  )
  
  # 2. Call SQL Console Module (Matches UI ID: "tab_2")
  sqlConsoleServer(
    id = "tab_2", 
    db = db 
  )
  
  # 3. Call DataViz Module (Matches UI ID: "tab_3")
  # Correctly injected data variables without trailing calculation brackets
  datavizServer(
    id = "tab_3",
    results_data = reactive_results,
    stints_data = reactive_stints
  )
}

# Run the complete integrated application
shinyApp(ui = ui, server = server)
