dbCrudUI <- function(id, metables) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Edit the Database"),
    selectInput(ns("choosetable"), "Choose a table:", choices = metables),
    selectInput(ns("filter_col"), "Choose column to filter on", NULL),
    selectInput(ns("filter_val"), "Choose values to include", NULL, multiple = TRUE),
    dataTableOutput(ns("mytable")), 
    verbatimTextOutput(ns("pkoutput")),
    uiOutput(ns("fields")),
    
    # Action buttons
    actionButton(ns("submit"), "Submit"),
    actionButton(ns("new"), "New"),
    actionButton(ns("delete"), "Delete")
  )
}

# ==============================================================================
# MODULE SERVER
# ==============================================================================
dbCrudServer <- function(id, db) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive database reader
    ReadData <- reactive({
      req(input$choosetable)
      tbl_name <- DBI::dbQuoteIdentifier(db, input$choosetable)
      DBI::dbGetQuery(db, paste0("SELECT * FROM ", tbl_name))
    })
    
    # Metadata fetchers
    GetTableMetadata <- reactive({
      req(input$choosetable) 
      fields <- dbListFields(db, input$choosetable)
      list(fields = fields)
    })  
    
    current_pk <- reactive({
      req(input$choosetable)
      query <- sprintf("PRAGMA table_info(%s);", dbQuoteIdentifier(db, input$choosetable))
      table_metadata <- dbGetQuery(db, query)
      pk_columns <- table_metadata$name[table_metadata$pk > 0]
      if (length(pk_columns) == 0) NULL else pk_columns
    })
    
    output$pkoutput <- renderPrint({
      pk <- current_pk()
      if (is.null(pk)) {
        "Primary key: No primary key defined"
      } else {
        paste0("Primary key is: ", paste(pk, collapse = ", "))
      }
    })
    
    # Dynamic dropdown updates
    observeEvent(input$choosetable, {
      df <- ReadData()
      updateSelectInput(session, "filter_col", choices = c("No Filter" = "", names(df)), selected = "")
    })
    
    observeEvent(input$filter_col, {
      df <- ReadData()
      if (!is.null(input$filter_col) && input$filter_col != "" && input$filter_col %in% names(df)) {
        unique_vals <- unique(as.character(df[[input$filter_col]]))
        updateSelectInput(session, "filter_val", choices = unique_vals, selected = NULL)
      } else {
        updateSelectInput(session, "filter_val", choices = NULL, selected = NULL)
      }
    })
    
    # Reactive filter expression
    FilteredData <- reactive({
      # Force dependency on changes from mutations
      input$submit
      input$delete
      
      df <- ReadData()
      if (!is.null(input$filter_col) && input$filter_col != "" && !is.null(input$filter_val) && length(input$filter_val) > 0) {
        df <- df %>% filter(as.character(.data[[input$filter_col]]) %in% input$filter_val)
      }
      df
    })
    
    # CRUD execution mechanics
    CastData <- function(data) {
      dbfields <- dbListFields(db, input$choosetable)
      matched_data <- data[intersect(names(data), dbfields)]
      as.data.frame(matched_data, stringsAsFactors = FALSE)
    }
    
    CreateData <- function(data) {
      req(input$choosetable)
      formatted_data <- CastData(data)
      dbAppendTable(db, input$choosetable, formatted_data)
    }
    
    UpdateData <- function(data) {
      req(input$choosetable)
      pk <- current_pk()
      if (is.null(pk)) stop("Cannot update a table without a primary key.")
      formatted_data <- CastData(data)
      valid_cols <- dbListFields(db, input$choosetable)
      update_cols <- setdiff(intersect(names(formatted_data), valid_cols), pk)
      if (length(update_cols) == 0) stop("No matching columns found to update.")
      
      set_clause <- paste0(dbQuoteIdentifier(db, update_cols), " = ?", collapse = ", ")
      where_clause <- paste0(dbQuoteIdentifier(db, pk), " = ?", collapse = " AND ")
      tbl_name <- dbQuoteIdentifier(db, input$choosetable)
      
      query <- paste0("UPDATE ", tbl_name, " SET ", set_clause, " WHERE ", where_clause)
      params <- c(unname(as.list(formatted_data[update_cols])), unname(as.list(formatted_data[pk])))
      dbExecute(db, query, params = params)
    }
    
    DeleteData <- function(data) {
      req(input$choosetable)
      pk <- current_pk()
      if (is.null(pk)) stop("Cannot delete from a table without a primary key.")
      
      formatted_data <- CastData(data)
      where_clause <- paste0(dbQuoteIdentifier(db, pk), " = ?", collapse = " AND ")
      tbl_name <- DBI::dbQuoteIdentifier(db, input$choosetable)
      
      query <- paste0("DELETE FROM ", tbl_name, " WHERE ", where_clause)
      params <- unname(as.list(formatted_data[pk]))
      dbExecute(db, query, params = params)
    }
    
    formData <- reactive({
      req(input$choosetable)
      db %>% tbl(input$choosetable) %>% head %>% collect %>% lapply(type_sum) %>% unlist
    })
    
    getFormData <- function() {
      fieldNames <- names(formData())
      values <- lapply(fieldNames, function(nm) {
        val <- input[[paste0("field", nm)]]
        if (is.null(val)) return(NA)
        val
      })
      names(values) <- fieldNames
      as.data.frame(values, stringsAsFactors = FALSE)
    }
    
    # Dynamic form generation
    output$fields <- renderUI({
      fieldNames <- names(formData())
      fieldTypes <- unname(formData())
      selections <- vector("list", length(fieldNames))
      for (i in seq_len(length(fieldNames))) {
        nm <- fieldNames[i]
        id <- session$ns(paste0("field", nm)) # CRITICAL: session$ns needed here
        selections[[i]] <- box(width = 3,
                               switch(fieldTypes[i],
                                      int = numericInput(id, nm, NULL),
                                      chr = textInput(id, nm, NULL)
                               )
        )
      }
      selections
    }) 
    
    current_row_data <- reactiveVal(NULL)
    
    UpdateInputs <- function(data, session) {
      col_names <- colnames(data)
      for (col in col_names) {
        input_id <- paste0("field", col)
        val <- data[[col]][1]
        if (is.logical(val)) {
          updateCheckboxInput(session, input_id, value = nzchar(val) && val)
        } else if (is.numeric(val)) {
          updateNumericInput(session, input_id, value = val)
        } else {
          updateTextInput(session, input_id, value = as.character(val))
        }
      }
    }
    
    CreateDefaultRecord <- function() {
      fieldNames <- names(formData())
      fieldTypes <- unname(formData())
      defaults <- lapply(fieldTypes, function(type) {
        switch(type, int = NA_real_, dbl = NA_real_, chr = "", "")
      })
      names(defaults) <- fieldNames
      as.data.frame(defaults, stringsAsFactors = FALSE)
    }
    
    # Event Observers
    observeEvent(input$submit, {
      if (!is.null(current_row_data())) {
        UpdateData(getFormData())
      } else {
        CreateData(getFormData())
        UpdateInputs(CreateDefaultRecord(), session)
      }
      current_row_data(NULL) 
    })
    
    observeEvent(input$new, {
      current_row_data(NULL)
      UpdateInputs(CreateDefaultRecord(), session)
    })
    
    observeEvent(input$delete, {
      req(current_row_data())
      DeleteData(getFormData())
      UpdateInputs(CreateDefaultRecord(), session)
      current_row_data(NULL)
    })
    
    observeEvent(input$mytable_rows_selected, {
      if (length(input$mytable_rows_selected) > 0) {
        all_data <- FilteredData() # Pull from filtered view 
        selected_row <- all_data[input$mytable_rows_selected, , drop = FALSE]
        current_row_data(selected_row)
        UpdateInputs(selected_row, session)
      }
    })
    
    output$mytable <- DT::renderDataTable({
      FilteredData()
    }, server = FALSE, selection = "single", colnames = unname(GetTableMetadata()$fields))
    
  })
}