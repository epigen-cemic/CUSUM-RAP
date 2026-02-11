#' @title CUSUM Analysis Module - UI
#'
#' @description
#' Generates the user interface for the CUSUM analysis module. This includes
#' a sidebar for configuration (file upload, geographic level, CUSUM parameters)
#' and a main panel with tabs for the Heatmap, Detailed View, and Data Table.
#'
#' @param id Character string. The namespace identifier for this module.
#'           Must match the id passed to \code{cusumServer}.
#'
#' @return A Shiny UI object (tagList) containing the sidebarLayout.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- fluidPage(cusumUI("main_analysis"))
#' }
cusumUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Configuration"),
      
      fileInput(ns("file_upload"), "Upload Rumor CSV", 
                accept = c(".csv", "text/csv", "text/comma-separated-values")),
      
      # CHANGE HERE: Set choices to NULL initially
      selectInput(ns("geo_level"), "Geographic Level:",
                  choices = NULL, 
                  selected = NULL),
      
      # ... rest of your UI inputs ...
      numericInput(ns("param_h"), "Threshold (h):", value = 3, step = 0.1),
      numericInput(ns("param_k"), "Reference (k):", value = 1.04, step = 0.01),
      numericInput(ns("baseline_weeks"), "Baseline Length (Weeks):", value = 52),
      
      hr(),
      actionButton(ns("run_analysis"), "Run Analysis", class = "btn-primary", width = "100%")
    ),
    mainPanel(
      # ... rest of main panel ...
      uiOutput(ns("error_message")),
      tabsetPanel(
        tabPanel("Overview", plotOutput(ns("plot_heatmap"), height = "600px")),
        tabPanel("Detailed View", 
                 br(),
                 fluidRow(
                   column(6, selectInput(ns("unit_selector"), "Select Location:", choices = NULL)),
                   column(6, downloadButton(ns("download_single_plot"), "Download Plot", class = "pull-right"))
                 ),
                 plotOutput(ns("plot_series")),
                 plotOutput(ns("plot_cusum_process"))
        ),
        tabPanel("Data", downloadButton(ns("download_data"), "Download CSV"), tableOutput(ns("table_preview")))
      )
    )
  )
}

#' @title CUSUM Analysis Module - Server Logic
#'
#' @description
#' Handles the backend logic for the CUSUM analysis. This includes:
#' \enumerate{
#'   \item Validating the uploaded CSV file structure.
#'   \item Aggregating data to the selected geographic level.
#'   \item Running the CUSUM algorithm (baseline vs detection period).
#'   \item Rendering plots (Heatmap, Series, Process) and data tables.
#' }
#'
#' @param id Character string. The namespace identifier for this module.
#'           Must match the id passed to \code{cusumUI}.
#'
#' @return The function does not return a value to the global environment,
#'         but executes the module server logic.
#'
#' @import shiny
#' @import dplyr
#' @import ggplot2
#'
#' @export
cusumServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ---------------------------------------------------------
    # 0. INITIALIZATION: Read Config & Populate Dropdown
    # ---------------------------------------------------------
    observe({
      # 1. Read the config file safely inside the server
      config_path <- here::here("www/config.json")
      
      # Check if file exists to avoid crashing
      if(file.exists(config_path)){
        config_data <- jsonlite::fromJSON(config_path)
        
        # 2. Extract levels
        # Assuming you want Argentina, or you can make this dynamic later
        raw_choices <- config_data$Argentina$levels 
        
        # Create named vector: c("Fraction" = "fraction", "Province" = "province")
        clean_choices <- raw_choices
        names(clean_choices) <- tools::toTitleCase(raw_choices)
        
        # 3. Update the UI
        updateSelectInput(session, "geo_level",
                          choices = clean_choices,
                          selected = "province") # Set a default if available
      }
    })
    
    # ---------------------------------------------------------
    # 1. Reactive: Read, Validate, and Prepare Data
    # ---------------------------------------------------------
    analyzed_data <- eventReactive(input$run_analysis, {
      req(input$file_upload)
      req(input$geo_level) # Wait until the dropdown is populated
      
      # --- MAPPING LOGIC ---
      selected_level <- input$geo_level
      
      # Translate UI selection ("fraction") to internal ID ("censal_censal_fraction")
      internal_geo_level <- switch(selected_level,
                                   "fraction" = "censal_censal_fraction", 
                                   selected_level # Default
      )
      
      tryCatch({
        raw_df <- read_api_pop_output(input$file_upload$datapath)
        
        # Validation
        if (!"n_cases" %in% names(raw_df)) validate(need(FALSE, "Error: Column 'n_cases' missing."))
        if (!is.numeric(raw_df$n_cases))   validate(need(FALSE, "Error: 'n_cases' must be numeric."))
        
        # --- DATA PREPARATION (Using generic CSV levels) ---
        prepared_df <- prepare_weekly_data_geo(
          df = raw_df, 
          location_level = internal_geo_level, 
          
          # Mapping generic CSV headers (level1, level2...) to concepts
          col_country         = "country",
          col_province        = "level1",    # Province
          col_department      = "level2",    # Department
          col_censal_fraction = "level3",    # Fraction
          
          col_yearweek        = "week",
          col_cases           = "n_cases"
        )
        
        # Check baseline length
        max_week <- max(prepared_df$time_index, na.rm = TRUE)
        if (max_week < input$baseline_weeks) {
          showNotification("Warning: Data shorter than baseline.", type = "warning")
        }
        
        # Run CUSUM
        cutoff_week <- max_week - input$baseline_weeks
        
        results <- run_cusum_all_units(
          df              = prepared_df,
          unit_var        = "analysis_unit_id",
          baseline_filter = (prepared_df$time_index <= cutoff_week), 
          detect_filter   = (prepared_df$time_index > cutoff_week),
          k               = input$param_k,
          h               = input$param_h
        )
        
        return(results)
        
      }, error = function(e) {
        validate(need(FALSE, paste("Error:", e$message)))
      })
    })
    
    # ---------------------------------------------------------
    # 2. Outputs
    # ---------------------------------------------------------
    
    observeEvent(analyzed_data(), {
      units <- unique(analyzed_data()$analysis_unit_id)
      updateSelectInput(session, "unit_selector", choices = units)
    })
    
    output$plot_heatmap <- renderPlot({
      req(analyzed_data())
      plot_cusum_alarms_overview(analyzed_data())
    })
    
    output$plot_series <- renderPlot({
      req(analyzed_data(), input$unit_selector)
      df_unit <- analyzed_data() %>% dplyr::filter(analysis_unit_id == input$unit_selector)
      plot_cusum_series_unit(df_unit, unit_label = input$unit_selector)
    })
    
    output$plot_cusum_process <- renderPlot({
      req(analyzed_data(), input$unit_selector)
      df_unit <- analyzed_data() %>% dplyr::filter(analysis_unit_id == input$unit_selector)
      plot_cusum_process_unit(df_unit, unit_label = input$unit_selector, h = input$param_h)
    })
    
    output$table_preview <- renderTable({
      req(analyzed_data())
      head(analyzed_data(), 20)
    })
    
    output$download_data <- downloadHandler(
      filename = function() { paste0("cusum_results_", input$geo_level, ".csv") },
      content = function(file) { write.csv(analyzed_data(), file, row.names = FALSE) }
    )
  })
}