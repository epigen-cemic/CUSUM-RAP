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
  
  fluidPage(
    shinyjs::useShinyjs(),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "config.css")
    ),     # Link to the external CSS file located in the www folder
    
    # TOP NAVIGATION BAR
    fluidRow(
      div(class = "custom-navbar",
          div(class = "dropdown",
              tags$a("Files"),
              div(class = "dropdown-content",
                  tags$a("New File"),
                  actionLink(ns("menu_load"), "Load Prepared Data"),
                  downloadLink(ns("menu_save"), "Save Prepared Data"),
                  
                  tags$hr(style = "border-color: black; margin: 0; border-width: 2px;"),
                  
                  actionLink(ns("menu_upload"), "Upload RAW Data"),
                  actionLink(ns("menu_remove"), "Clear Memory")
              )
          ),
          tags$a("Selection"),
          tags$a("Help")
      )
    ),
    
    br(),
    
    # MAIN LAYOUT (Sidebar + Content)
    fluidRow(
      # SIDEBAR
      column(3,
             div(class = "sidebar-dark",
                 tags$label(class = "control-label", "Geographic Level"),
                 selectInput(ns("geo_level"), label = NULL, choices = NULL, selected = NULL, width = "100%"),
                 
                 # Dynamic dropdown for specific locations (hidden unless data is loaded)
                 uiOutput(ns("dynamic_target_ui")),
                 
                 
                 br(),
                 tags$label(class = "control-label", "Baseline Configuration"),
                 numericInput(ns("param_weeks"), "Detection Period (Weeks):", 
                              value = 52, min = 52, max = 104, width = "100%"),
                 
                 
                 br(),
                 tags$label(class = "title-label", "Variables"),
                 br(),
                 
                 
                 tags$label(class = "control-label", "Expected Frequency (\u03BC0)"),
                 # 2. Toggle for Automatic vs Manual
                 radioButtons(ns("mu_method"), label = NULL, 
                              choices = c("Automatic (Poisson GLM)" = "auto", 
                                          "Manual Input" = "manual")),
                 
                 # 3. Conditional panel that only shows if 'manual' is selected
                 conditionalPanel(
                   condition = paste0("input['", ns("mu_method"), "'] == 'manual'"),
                   tags$label(class = "control-label", "Enter Reference frequency (\u03BC0):"),
                   tags$div(class = "input-overlay-wrapper",
                            numericInput(ns("param_mu"), label = NULL, value = 10, width = "100%"),
                            tags$span("per 100.000", class = "input-overlay-text")
                   )
                 ),
                 
                 br(),
                 tags$label(class = "control-label", "CUSUM Parameters"),
                 
                 # 1. ARL0
                 div(class = "variable-label", tags$label(class = "control-label", "Target ARL0 :"), 
                     numericInput(ns("param_arl0"), label = NULL, value = NULL)),
                 
                 div(style = "min-height: 25px;"),
                 
                 # 2. RR (Relative Risk) - Replaces k
                 div(class = "variable-label", tags$label(class = "control-label", "RR (Relative Risk):"), 
                     numericInput(ns("param_rr"), label = NULL, value = NULL, step = 0.01)),
                 
                 # Shows calculated k based on RR and mu0
                 div(style = "min-height: 25px;", uiOutput(ns("calc_k_text"))),
                 
                 # 3. h 
                 div(class = "variable-label", tags$label(class = "control-label", "h (Threshold):"), 
                     numericInput(ns("param_h"), label = NULL, value = NULL, step = 0.001)),
                 
                 div(style = "min-height: 25px;", uiOutput(ns("rec_text_h"))),
                 
                 br(),
                 
                 br(),
                 actionButton(ns("run_analysis"), "Run Analysis", class = "btn-default", style = "width: 100%; color: black;")
             )
      ),
      
      # --- MAIN PANEL (Updated with separated tabs) ---
      column(9,
             uiOutput(ns("error_message")),
             div(class = "main-tabs",
                 tabsetPanel(
                   tabPanel("Overview", br(), plotOutput(ns("plot_heatmap"), height = "600px")),
                   
                   tabPanel("Detailed View", 
                            br(),
                            fluidRow(
                              column(6, selectInput(ns("unit_selector"), "Select Location:", choices = NULL)),
                              column(6, div(class = "pull-right",
                                            downloadButton(ns("download_series_plot"), "Download Bar Plot", class = "btn-info"),
                                            downloadButton(ns("download_process_plot"), "Download Trends", class = "btn-warning")
                              ))
                            ),
                            plotOutput(ns("plot_series")),
                            plotOutput(ns("plot_cusum_process"))
                   ),
                   
                   # Tab for the final CUSUM results
                   tabPanel("Analysis Results", 
                            br(),
                            DT::DTOutput(ns("table_preview")),
                            br(),
                            # The download button starts disabled
                            shinyjs::disabled(
                              downloadButton(ns("download_data"), "Download Results CSV", class = "btn-default")
                            )
                   ),
                   
                   # Restored separate tab for the gap-filled data
                   tabPanel("Prepared Data",
                            # New stylized legend instead of the download button
                            div(style = "background-color: #e9ecef; padding: 15px; border-left: 5px solid #4a4a4a; margin-bottom: 20px;",
                                tags$p(tags$strong("Pro Tip:"), "To save this formatted dataset for future use, go to the top menu: ", 
                                       tags$span(style = "font-style: italic; color: #333;", "Files > Save Prepared Data"), "."),
                                tags$p(style = "font-size: 0.9em; margin-bottom: 0;", 
                                       "This will export a .csv that includes all filled weeks and geographic levels.")
                            ),
                            textOutput(ns("rows_added_info")),
                            DT::DTOutput(ns("table_prepared"))
                   )
                 )
             )
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
    ns <- session$ns
    
    # ---------------------------------------------------------
    # 0. INITIALIZATION
    # ---------------------------------------------------------
    hierarchy_levels_list <- reactiveVal(c("country", "level1", "level2", "level3")) 
    overlap_preference <- reactiveVal(NULL)
    
    observe({
      config_path <- here::here("www/config.json")
      if(file.exists(config_path)){
        config_data <- jsonlite::fromJSON(config_path)
        raw_choices <- config_data$Argentina$levels 
        hierarchy_levels_list(raw_choices)
        clean_choices <- raw_choices
        names(clean_choices) <- tools::toTitleCase(raw_choices)
        updateSelectInput(session, "geo_level", choices = clean_choices, selected = raw_choices[2])
      }
    })
    
    # ---------------------------------------------------------
    # 1. DATA INPUT (UPLOAD / LOAD / REMOVE)
    # ---------------------------------------------------------
    observeEvent(input$menu_upload, {
      showModal(modalDialog(
        title = "Upload Data",
        fileInput(ns("file_upload"), "Select CSV File(s)", multiple = TRUE, accept = c(".csv")),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$menu_remove, {
      shinyjs::reset("file_upload") 
      showNotification("Data removed.", type = "message")
    })
    
    observeEvent(input$menu_load, {
      showModal(modalDialog(
        title = "Load Prepared Data",
        p("Select a previously saved 'Prepared Data' file."),
        fileInput(ns("file_load_temp"), "Select CSV File", accept = c(".csv")),
        easyClose = TRUE
      ))
    })
    
    output$menu_save <- downloadHandler(
      filename = function() { paste0("Prepared_Data_", Sys.Date(), ".csv") },
      content = function(file) {
        req(active_dataset()) 
        write.csv(active_dataset(), file, row.names = FALSE)
      }
    )
    
    # ---------------------------------------------------------
    # 2. RAW DATA PROCESSING & CONFLICT RESOLUTION
    # ---------------------------------------------------------
    raw_combined_data <- reactive({
      req(input$file_upload)
      tryCatch({
        file_paths <- setNames(input$file_upload$datapath, seq_along(input$file_upload$datapath))
        raw_df <- purrr::map_dfr(file_paths, read_api_pop_output, .id = "file_index")
        names(raw_df) <- tolower(names(raw_df))
        raw_df$file_index <- as.numeric(raw_df$file_index)
        return(raw_df)
      }, error = function(e) {
        showNotification(paste("Error reading files:", e$message), type = "error")
        return(NULL)
      })
    })
    
    observeEvent(input$file_upload, {
      req(input$file_upload)
      removeModal() 
      
      if (nrow(input$file_upload) > 1) {
        overlap_preference(NULL) 
        showModal(modalDialog(
          title = "Multiple Files Detected",
          p("How should we resolve overlapping location/week data?"),
          radioButtons(ns("modal_overlap_choice"), label = NULL,
                       choices = c("Keep Newest File Information" = "new",
                                   "Keep Oldest File Information" = "old",
                                   "Add Together (Sum)" = "sum"),
                       selected = "new"),
          footer = tagList(actionButton(ns("confirm_overlap"), "Confirm & Continue", class = "btn-primary")),
          easyClose = FALSE
        ))
      } else {
        overlap_preference("sum")
      }
    })
    
    observeEvent(input$confirm_overlap, {
      overlap_preference(input$modal_overlap_choice)
      removeModal()
    })
    
    # ---------------------------------------------------------
    # 3. DYNAMIC UI & TARGET LOCATIONS
    # ---------------------------------------------------------
    output$dynamic_target_ui <- renderUI({
      req(raw_combined_data())
      selectizeInput(ns("target_locations"), "Specific Locations:",
                     choices = NULL, multiple = TRUE,
                     options = list(placeholder = 'Select locations...'),
                     width = "100%")
    })
    
    observeEvent(c(raw_combined_data(), input$geo_level), {
      req(raw_combined_data(), input$geo_level)
      raw_df <- raw_combined_data()
      h_levels <- hierarchy_levels_list()
      generic_map <- c("country", "level1", "level2", "level3", "level4")
      target_col <- generic_map[which(h_levels == input$geo_level)]
      
      if (length(target_col) > 0 && target_col %in% names(raw_df)) {
        locs <- unique(raw_df[[target_col]])
        locs <- sort(locs[!is.na(locs) & locs != ""])
        updateSelectizeInput(session, "target_locations", choices = locs, server = TRUE) 
      }
    })
    
    # ---------------------------------------------------------
    # 4. DATA PREPARATION (The Gatekeeper)
    # ---------------------------------------------------------
    prepared_target_data <- reactive({
      req(raw_combined_data(), input$geo_level, input$target_locations, overlap_preference())
      
      if (identical(input$target_locations, "") || length(input$target_locations) == 0) {
        return(NULL)
      }
      
      h_levels <- hierarchy_levels_list()
      generic_map <- c("country", "level1", "level2", "level3", "level4")
      level_depth <- which(h_levels == input$geo_level)
      
      process_target_data(
        raw_df           = raw_combined_data(),
        target_locations = input$target_locations,
        target_col       = generic_map[level_depth],
        req_cols         = generic_map[1:level_depth],
        overlap_method   = overlap_preference(),
        hierarchy_levels = h_levels,
        selected_level   = input$geo_level
      )
    })
    
    # ---------------------------------------------------------
    # 4.5 SMART PARAMETER RECOMMENDATIONS & CALCULATIONS
    # ---------------------------------------------------------
    
    # Helper to peek at what k will be if using manual inputs
    current_k <- reactive({
      req(input$param_rr)
      if (input$mu_method == "manual") {
        req(input$param_mu)
        return(calculate_k_from_rr(input$param_rr, input$param_mu))
      } else {
        # If auto, we calculate during run_analysis
        return(NULL)
      }
    })
    
    # Render calculated 'k' to UI
    output$calc_k_text <- renderUI({
      k_val <- current_k()
      if (!is.null(k_val)) {
        tags$small(style = "color: #f9ff42; font-style: italic; display: block; text-align: right; margin-top: -10px; margin-bottom: 10px;", 
                   paste("Calculated k:", round(k_val, 3)))
      } else {
        tags$small(style = "color: #cccccc; font-style: italic; display: block; text-align: right; margin-top: -10px; margin-bottom: 10px;", 
                   "k will be calculated automatically.")
      }
    })
    
    # Render recommended 'h'
    output$rec_text_h <- renderUI({
      # Needs a calculated k to give an accurate h recommendation
      req(input$param_arl0, current_k())
      rec_val <- recommend_h(input$param_arl0, current_k())
      
      if (!is.na(rec_val)) {
        tags$small(style = "color: #f9ff42; font-style: italic; display: block; text-align: right; margin-top: -10px; margin-bottom: 10px;", 
                   paste("Recommended h:", rec_val))
      }
    })
    
    active_dataset <- reactive({
      if (isTruthy(input$file_load_temp)) {
        df <- read.csv(input$file_load_temp$datapath)
        names(df) <- tolower(names(df))
        return(df)
      } else {
        return(prepared_target_data())
      }
    })
    
    # ---------------------------------------------------------
    # 5. CUSUM MATH ENGINE
    # ---------------------------------------------------------
    analyzed_data <- eventReactive(input$run_analysis, {
      # Require the new param_rr instead of param_k
      req(active_dataset(), input$param_h, input$param_rr, input$param_weeks)
      
      prepared_df <- active_dataset() 
      if (nrow(prepared_df) == 0) return(NULL)
      
      max_week_index <- max(prepared_df$time_index, na.rm = TRUE)
      window_size <- as.numeric(input$param_weeks)
      
      if (max_week_index < window_size) {
        validate(sprintf("Error: You requested a total analysis period of %d weeks, but your file only contains %d weeks.", 
                         window_size, max_week_index))
      }
      
      start_week <- max(0, max_week_index - window_size)
      
      if (input$mu_method == "manual") {
        req(input$param_mu)
        final_mu <- input$param_mu
      } else {
        final_mu <- get_phase1_baseline(prepared_df, window_size)
      }
      
      # Calculate k dynamically using the new function
      calculated_k <- calculate_k_from_rr(input$param_rr, final_mu)
      
      res <- run_cusum_all_units(
        df              = prepared_df,
        unit_var        = "analysis_unit_id",
        baseline_filter = function(d) d$time_index > start_week,
        detect_filter   = function(d) d$time_index > start_week,
        k               = calculated_k,
        h               = input$param_h,
        fixed_mu        = final_mu,
        reset           = TRUE
      )
      
      # Attach the specific calculated k safely so plotting functions can retrieve it
      if (length(calculated_k) == 1) {
        res$k_value <- calculated_k
      } else if (is.vector(calculated_k) && !is.null(names(calculated_k))) {
        res$k_value <- calculated_k[res$analysis_unit_id]
      } else {
        res$k_value <- calculated_k[1]
      }
      
      return(res)
    })
    
    observeEvent(analyzed_data(), {
      units <- unique(analyzed_data()$analysis_unit_id)
      updateSelectInput(session, "unit_selector", choices = units)
    })
    
    # ---------------------------------------------------------
    # 6. REACTIVE PLOT OBJECTS (The Logic Hub)
    # ---------------------------------------------------------
    heatmap_obj <- reactive({
      req(analyzed_data())
      plot_cusum_alarms_overview(analyzed_data())
    })
    
    series_plot_obj <- reactive({
      req(analyzed_data(), input$unit_selector)
      df_unit <- analyzed_data() %>% 
        dplyr::filter(analysis_unit_id == input$unit_selector)
      
      plot_cusum_series_unit(df_unit, unit_label = input$unit_selector)
    })
    
    process_plot_obj <- reactive({
      req(analyzed_data(), input$unit_selector)
      df_unit <- analyzed_data() %>% 
        dplyr::filter(analysis_unit_id == input$unit_selector)
      
      # Extract the k-value that was specifically calculated for this unit
      unit_k <- unique(df_unit$k_value)[1]
      
      plot_cusum_process_unit(df_unit, 
                              unit_label = input$unit_selector, 
                              h = input$param_h,
                              k = unit_k,
                              arl0 = input$param_arl0)
    })
    
    # ---------------------------------------------------------
    # 7. RENDERING & BUTTON CONTROL
    # ---------------------------------------------------------
    output$plot_heatmap <- renderPlot({ heatmap_obj() })
    output$plot_series  <- renderPlot({ series_plot_obj() })
    output$plot_cusum_process <- renderPlot({ process_plot_obj() })
    
    observe({
      if (isTruthy(analyzed_data())) {
        shinyjs::enable("download_data")
        shinyjs::enable("download_series_plot")
        shinyjs::enable("download_process_plot")
      } else {
        shinyjs::disable("download_data")
        shinyjs::disable("download_series_plot")
        shinyjs::disable("download_process_plot")
      }
    })
    
    # ---------------------------------------------------------
    # 8. DOWNLOAD HANDLERS (Connected to Reactives)
    # ---------------------------------------------------------
    output$download_series_plot <- downloadHandler(
      filename = function() {
        clean_name <- sub(".*\\|", "", input$unit_selector)
        paste0("CUSUM_Bar_plot_", clean_name, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = series_plot_obj(), 
                        device = "png", width = 16, height = 7, dpi = 300)
      }
    )
    
    output$download_process_plot <- downloadHandler(
      filename = function() {
        clean_name <- sub(".*\\|", "", input$unit_selector)
        paste0("CUSUM_Trends_", clean_name, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = process_plot_obj(), 
                        device = "png", width = 16, height = 7, dpi = 300)
      }
    )
    
    output$table_preview <- DT::renderDT({
      req(analyzed_data())
      
      df <- analyzed_data()
      
      if ("epi_date" %in% names(df)) {
        df$epi_date <- format(as.Date(df$epi_date), "%G-W%V")
      }
      
      DT::datatable(
        df,
        options = list(
          pageLength = 25, 
          scrollX = TRUE,
          columnDefs = list(list(className = 'dt-left', targets = "_all")),
          order = list(list(1, 'desc'))
        ),
        rownames = FALSE
      )
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("CUSUM_Results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- analyzed_data()
        
        if ("epi_date" %in% names(df)) {
          df$epi_date <- format(as.Date(df$epi_date), "%G-W%V")
        }
        
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}