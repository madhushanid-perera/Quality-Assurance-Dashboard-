library(shiny)
library(shinydashboard)
library(qcc)
library(DT)
library(readxl)
library(openxlsx)

# Increase file upload limit for larger datasets
options(shiny.maxRequestSize = 30 * 1024^2)

ui <- dashboardPage(
  dashboardHeader(title = "QA Dashboard - Phase I & II"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Phase I - Application of Charts", tabName = "phase1", icon = icon("cogs")),
      menuItem("Phase II - Operation of Charts", tabName = "phase2", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # ========== PHASE I TAB ==========
      tabItem(tabName = "phase1",
              fluidRow(
                box(width = 6, title = "1. Upload Dataset", status = "primary", solidHeader = TRUE,
                    fileInput("file1", "Choose File (CSV, Excel, TXT)", 
                              accept = c(".csv", ".xlsx", ".xls", ".txt")),
                    checkboxInput("header", "First row contains headers", TRUE),
                    hr(),
                    p("After uploading, select your measurement columns below.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Data Preview", status = "info",
                    DTOutput("preview"))
              ),
              fluidRow(
                box(width = 4, title = "2. Select Measurement Columns", status = "warning", solidHeader = TRUE,
                    uiOutput("col_selector")
                ),
                box(width = 4, title = "3. Subgroup Selection for Phase I", status = "warning", solidHeader = TRUE,
                    numericInput("p1_count", "Number of samples for Phase I (control limits):", 
                                 value = 25, min = 2),
                    helpText("These samples will establish the control limits.")
                ),
                box(width = 4, title = "4. Chart Type Selection", status = "warning", solidHeader = TRUE,
                    checkboxGroupInput("chart_types", "Select Chart Type(s):", 
                                       choices = c("X-bar Chart" = "xbar", 
                                                   "R Chart" = "R"),
                                       selected = c("xbar", "R")),
                    helpText("You can select one or both charts.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Phase I Control Charts (Trial Limits)", 
                    status = "success", solidHeader = TRUE,
                    uiOutput("p1_charts"))
              )
      ),
      
      # ========== PHASE II TAB ==========
      tabItem(tabName = "phase2",
              fluidRow(
                box(width = 12, title = "Phase II - Process Monitoring", 
                    status = "primary", solidHeader = TRUE,
                    p("This section shows the full dataset with Phase I control limits applied."),
                    p("You can also add individual values below to monitor new observations.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Enter New Individual Values", 
                    status = "warning", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("new_value_inputs_comma"),
                    helpText("Enter values separated by commas in the same order as your selected measurement columns."),
                    actionButton("add_values", "Add New Observation", icon = icon("plus"), 
                                 class = "btn-success"),
                    actionButton("reset_values", "Reset New Observations", icon = icon("refresh"), 
                                 class = "btn-danger"),
                    hr(),
                    h5("New Observations Added:"),
                    DTOutput("new_obs_table")
                )
              ),
              fluidRow(
                box(width = 12, title = "Phase II Control Charts (Full Data + New Observations)", 
                    status = "success", solidHeader = TRUE,
                    uiOutput("p2_charts"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store new observations
  new_observations <- reactiveVal(data.frame())
  
  # 1. Reactive: Read the uploaded file
  raw_data <- reactive({
    req(input$file1)
    
    file_ext <- tools::file_ext(input$file1$name)
    
    tryCatch({
      if(file_ext == "csv") {
        read.csv(input$file1$datapath, header = input$header)
      } else if(file_ext %in% c("xlsx", "xls")) {
        read_excel(input$file1$datapath, col_names = input$header)
      } else if(file_ext == "txt") {
        read.table(input$file1$datapath, header = input$header, sep = "\t")
      } else {
        NULL
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
  })
  
  # 2. UI: Generate column checkboxes based on uploaded data
  output$col_selector <- renderUI({
    req(raw_data())
    cols <- names(raw_data())
    # Default selection: all numeric columns
    numeric_cols <- cols[sapply(raw_data(), is.numeric)]
    checkboxGroupInput("selected_cols", "Select Measurement Columns:", 
                       choices = cols, selected = numeric_cols)
  })
  
  # 3. UI: Data Preview Table
  output$preview <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # 4. Reactive: Process Phase 1 Data
  phase1_matrix <- reactive({
    req(raw_data(), input$selected_cols, input$p1_count)
    df <- raw_data()[, input$selected_cols, drop = FALSE]
    # Convert to numeric if needed
    df[] <- lapply(df, as.numeric)
    n_limit <- min(input$p1_count, nrow(df))
    as.matrix(df[1:n_limit, , drop = FALSE])
  })
  
  # 5. Reactive: Process Phase 2 Data (Original observations after Phase 1)
  phase2_original <- reactive({
    req(raw_data(), input$selected_cols, input$p1_count)
    df <- raw_data()[, input$selected_cols, drop = FALSE]
    df[] <- lapply(df, as.numeric)
    n_limit <- min(input$p1_count, nrow(df))
    
    if(nrow(df) > n_limit) {
      return(as.matrix(df[(n_limit + 1):nrow(df), , drop = FALSE]))
    } else {
      return(NULL)
    }
  })
  
  # 6. Reactive: Combined Phase 2 data (original + new observations)
  phase2_combined <- reactive({
    req(input$selected_cols)
    
    original <- phase2_original()
    new_obs <- new_observations()
    
    # If we have new observations
    if(nrow(new_obs) > 0) {
      new_matrix <- as.matrix(new_obs[, input$selected_cols, drop = FALSE])
      
      if(!is.null(original)) {
        return(rbind(original, new_matrix))
      } else {
        return(new_matrix)
      }
    } else {
      return(original)
    }
  })
  
  # 7. UI: Comma-separated input field for new values
  output$new_value_inputs_comma <- renderUI({
    req(input$selected_cols)
    
    column_names <- paste(input$selected_cols, collapse = ", ")
    
    tagList(
      h5(paste("Selected columns:", column_names)),
      textInput("new_values_comma", 
                label = "Enter values (comma-separated):", 
                value = "",
                placeholder = "e.g., 10.5, 12.3, 9.8"),
      helpText(paste("Enter", length(input$selected_cols), 
                     "values in order:", column_names))
    )
  })
  
  # 8. Observer: Add new observation from comma-separated input
  observeEvent(input$add_values, {
    req(input$selected_cols, input$new_values_comma)
    
    # Parse comma-separated values
    values_text <- trimws(input$new_values_comma)
    
    if(values_text == "") {
      showNotification("Please enter values before adding.", type = "warning")
      return()
    }
    
    # Split by comma and convert to numeric
    values <- tryCatch({
      as.numeric(unlist(strsplit(values_text, ",")))
    }, error = function(e) {
      showNotification("Error parsing values. Please ensure all values are numeric.", type = "error")
      return(NULL)
    })
    
    if(is.null(values)) return()
    
    # Check if number of values matches number of columns
    if(length(values) != length(input$selected_cols)) {
      showNotification(
        paste("Error: Expected", length(input$selected_cols), 
              "values but got", length(values), 
              ". Please enter exactly", length(input$selected_cols), "comma-separated values."),
        type = "error", duration = 10
      )
      return()
    }
    
    # Check for NA values
    if(any(is.na(values))) {
      showNotification("Error: Some values could not be converted to numbers. Please check your input.", 
                       type = "error")
      return()
    }
    
    # Create new row
    new_row <- data.frame(matrix(values, nrow = 1))
    colnames(new_row) <- input$selected_cols
    
    # Add to reactive value
    current_obs <- new_observations()
    new_observations(rbind(current_obs, new_row))
    
    # Clear input field
    updateTextInput(session, "new_values_comma", value = "")
    
    showNotification("New observation added successfully!", type = "message")
  })
  
  # 9. Observer: Reset new observations
  observeEvent(input$reset_values, {
    new_observations(data.frame())
    showNotification("New observations have been reset.", type = "message")
  })
  
  # 10. UI: Show table of new observations
  output$new_obs_table <- renderDT({
    new_obs <- new_observations()
    if(nrow(new_obs) > 0) {
      datatable(new_obs, options = list(pageLength = 5, dom = 't'))
    } else {
      datatable(data.frame(Message = "No new observations added yet."))
    }
  })
  
  # 11. Render: Phase 1 Charts
  output$p1_charts <- renderUI({
    req(input$chart_types)
    
    chart_outputs <- lapply(input$chart_types, function(chart_type) {
      plot_id <- paste0("p1_", chart_type, "_plot")
      box(width = 12, 
          title = ifelse(chart_type == "xbar", "X-bar Chart (Phase I)", "R Chart (Phase I)"),
          status = "info", solidHeader = TRUE,
          plotOutput(plot_id, height = "500px"))
    })
    
    do.call(tagList, chart_outputs)
  })
  
  # 12. Render: Phase 1 individual plots
  observe({
    req(input$chart_types)
    
    for(chart_type in input$chart_types) {
      local({
        ct <- chart_type
        plot_id <- paste0("p1_", ct, "_plot")
        
        output[[plot_id]] <- renderPlot({
          data <- phase1_matrix()
          req(data)
          
          y_lab <- if(ct == "xbar") "Sample Mean" else "Sample Range"
          chart_title <- if(ct == "xbar") "Phase I: X-bar Chart" else "Phase I: R Chart"
          
          qcc(data, type = ct, 
              xlab = "Sample", 
              ylab = y_lab, 
              title = chart_title)
        })
      })
    }
  })
  
  # 13. Render: Phase 2 Charts
  output$p2_charts <- renderUI({
    req(input$chart_types)
    
    chart_outputs <- lapply(input$chart_types, function(chart_type) {
      plot_id <- paste0("p2_", chart_type, "_plot")
      box(width = 12, 
          title = ifelse(chart_type == "xbar", "X-bar Chart (Phase II - Full Data)", 
                         "R Chart (Phase II - Full Data)"),
          status = "success", solidHeader = TRUE,
          plotOutput(plot_id, height = "500px"))
    })
    
    do.call(tagList, chart_outputs)
  })
  
  # 14. Render: Phase 2 individual plots
  observe({
    req(input$chart_types)
    
    for(chart_type in input$chart_types) {
      local({
        ct <- chart_type
        plot_id <- paste0("p2_", ct, "_plot")
        
        output[[plot_id]] <- renderPlot({
          p1_data <- phase1_matrix()
          p2_data <- phase2_combined()
          req(p1_data)
          
          y_lab <- if(ct == "xbar") "Sample Mean" else "Sample Range"
          chart_title <- if(ct == "xbar") "Phase II: X-bar Chart (Monitoring)" else "Phase II: R Chart (Monitoring)"
          
          # Phase 2 uses Phase 1's statistics to check new data
          qcc(p1_data, type = ct, newdata = p2_data, 
              xlab = "Sample", 
              ylab = y_lab, 
              title = chart_title)
        })
      })
    }
  })
}

shinyApp(ui, server)
