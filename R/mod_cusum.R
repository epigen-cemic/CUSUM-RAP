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
      
      selectInput(ns("geo_level"), "Geographic Level:",
                  choices = NULL, 
                  selected = NULL),
      
      hr(),
      h4("2. Baseline Settings"),
      
      radioButtons(ns("baseline_method"), "Baseline Calculation Method:",
                   choices = c("Automatic (Historical Data)" = "auto",
                               "Manual (Fixed Value)" = "manual"),
                   selected = "auto"),
      
      # Conditional panel for Manual Input
      conditionalPanel(
        condition = paste0("input['", ns("baseline_method"), "'] == 'manual'"),
        numericInput(ns("manual_mu_value"), "Fixed Expected Count (Mu):", 
                     value = NA, min = 0)
      ),
      
      # Conditional panel for Automatic Input
      # Replaced the manual numeric input with a fixed help text.
      conditionalPanel(
        condition = paste0("input['", ns("baseline_method"), "'] == 'auto'"),
        helpText("The baseline will be automatically calculated using all available historical data prior to the last 52 weeks.")
      ),
      
      hr(),
      h4("3. CUSUM Parameters"),
      
      numericInput(ns("param_h"), "Threshold (h):", 
                   value = NA, step = "0.001", min = 0), 
      
      numericInput(ns("param_k"), "Reference (k):", 
                   value = NA, step = "0.001", min = 0), 
      
      helpText("Note: Standard values are k=1.04 and h=2.26"), 
      
      br(),
      actionButton(ns("run_analysis"), "Run Analysis", class = "btn-primary", width = "100%")
    ),
    mainPanel(
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
      config_path <- here::here("www/config.json")
      
      if(file.exists(config_path)){
        config_data <- jsonlite::fromJSON(config_path)
        raw_choices <- config_data$Argentina$levels 
        
        clean_choices <- raw_choices
        names(clean_choices) <- tools::toTitleCase(raw_choices)
        
        updateSelectInput(session, "geo_level",
                          choices = clean_choices,
                          selected = "province") 
      }
    })
    
    # ---------------------------------------------------------
    # 1. Reactive: Read, Validate, and Prepare Data
    # ---------------------------------------------------------
    analyzed_data <- eventReactive(input$run_analysis, {
      req(input$file_upload)
      req(input$geo_level)
      
      # --- VALIDATION CHECKS ---
      
      if (is.na(input$param_h)) {
        showNotification("Error: Please enter a value for Threshold (h).", type = "error")
        validate(need(FALSE, "Waiting for Threshold (h) value..."))
      }
      
      if (is.na(input$param_k)) {
        showNotification("Error: Please enter a value for Reference (k).", type = "error")
        validate(need(FALSE, "Waiting for Reference (k) value..."))
      }
      
      fixed_mu_val <- NULL
      if (input$baseline_method == "manual") {
        if (is.na(input$manual_mu_value)) {
          showNotification("Error: Please enter a Fixed Expected Count (Mu).", type = "error")
          validate(need(FALSE, "Waiting for Manual Mu value..."))
        }
        fixed_mu_val <- input$manual_mu_value
      }
      
      # --- DATA LOADING & PREP ---
      selected_level <- input$geo_level
      internal_geo_level <- switch(selected_level,
                                   "fraction" = "censal_censal_fraction", 
                                   selected_level 
      )
      
      tryCatch({
        raw_df <- read_api_pop_output(input$file_upload$datapath)
        
        if (!"n_cases" %in% names(raw_df)) validate(need(FALSE, "Error: Column 'n_cases' missing."))
        if (!is.numeric(raw_df$n_cases))   validate(need(FALSE, "Error: 'n_cases' must be numeric."))
        
        prepared_df <- prepare_weekly_data_geo(
          df = raw_df, 
          location_level = internal_geo_level, 
          col_country         = "country",
          col_province        = "level1", 
          col_department      = "level2", 
          col_censal_fraction = "level3", 
          col_yearweek        = "week",
          col_cases           = "n_cases"
        )
        
        # --- CALCULATE 52-WEEK WINDOW ---
        max_week_index <- max(prepared_df$time_index, na.rm = TRUE)
        
        if (input$baseline_method == "auto" && max_week_index <= 52) {
          showNotification("Error: You need more than 52 weeks of historical data to calculate an automatic baseline.", type = "error")
          validate(need(FALSE, "Insufficient data for automatic baseline."))
        }
        
        # The detection period is strictly the latest 52 weeks in the dataset.
        # Everything before max_week_index - 52 becomes the baseline.
        cutoff_week <- max(0, max_week_index - 52)
        
        # --- RUN CUSUM ---
        results <- run_cusum_all_units(
          df              = prepared_df,
          unit_var        = "analysis_unit_id",
          # Using functions here ensures it applies correctly to each group individually!
          baseline_filter = function(d) d$time_index <= cutoff_week, 
          detect_filter   = function(d) d$time_index > cutoff_week,
          k               = input$param_k,
          h               = input$param_h,
          fixed_mu        = fixed_mu_val
        )
        
        return(results)
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
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