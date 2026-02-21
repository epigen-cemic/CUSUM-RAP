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
                 selectInput(ns("geo_level"), label = NULL, choices = NULL, selected = NULL),
                 
                 # Dynamic dropdown for specific locations (hidden unless data is loaded)
                 uiOutput(ns("dynamic_target_ui")),
                 
                 br(),
                 tags$label(class = "control-label", "Variables"),
                 br(),
                 
                 div(class = "variable-label", span("ARL 0 :"), numericInput(ns("param_arl0"), label = NULL, value = NA)),
                 div(class = "variable-label", span("k :"), numericInput(ns("param_k"), label = NULL, value = NA, step = 0.001)),
                 div(class = "variable-label", span("h :"), numericInput(ns("param_h"), label = NULL, value = NA, step = 0.001)),
                 
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
                                            downloadButton(ns("download_series_plot"), "Download Detailed Plot", class = "btn-info"),
                                            downloadButton(ns("download_process_plot"), "Download Process Plot", class = "btn-warning")
                              ))
                            ),
                            plotOutput(ns("plot_series")),
                            plotOutput(ns("plot_cusum_process"))
                   ),
                   
                   # Tab for the final CUSUM results
                   tabPanel("Analysis Results", 
                            br(),
                            tableOutput(ns("table_preview")), 
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
                     options = list(placeholder = 'Select locations...'))
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
      
      # Stop if location box is empty during transitions
      if (identical(input$target_locations, "") || length(input$target_locations) == 0) {
        return(NULL)
      }
      
      h_levels <- hierarchy_levels_list()
      generic_map <- c("country", "level1", "level2", "level3", "level4")
      level_depth <- which(h_levels == input$geo_level)
      
      # Process data using safe year-range logic (2000-2100) from functions_io.R
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
      req(active_dataset(), input$param_h, input$param_k, input$param_arl0)
      
      prepared_df <- active_dataset() 
      max_week_index <- max(prepared_df$time_index, na.rm = TRUE)
      cutoff_week <- max(0, max_week_index - 100)
      
      run_cusum_all_units(
        df              = prepared_df,
        unit_var        = "analysis_unit_id",
        baseline_filter = function(d) d$time_index <= cutoff_week, 
        detect_filter   = function(d) d$time_index > cutoff_week,
        k               = input$param_k,
        h               = input$param_h,
        fixed_mu        = input$param_arl0 
      )
    })
    
    # Update dropdown selector when analysis is complete
    observeEvent(analyzed_data(), {
      units <- unique(analyzed_data()$analysis_unit_id)
      updateSelectInput(session, "unit_selector", choices = units)
    })
    
    # ---------------------------------------------------------
    # 6. REACTIVE PLOT OBJECTS (The Logic Hub)
    # ---------------------------------------------------------
    # These create the plot objects once, so both the UI and Download button see the same thing.
    
    heatmap_obj <- reactive({
      req(analyzed_data())
      plot_cusum_alarms_overview(analyzed_data())
    })
    
    series_plot_obj <- reactive({
      req(analyzed_data(), input$unit_selector)
      df_unit <- analyzed_data() %>% 
        dplyr::filter(analysis_unit_id == input$unit_selector)
      
      # Calling the function defined in functions_plot.R
      plot_cusum_series_unit(df_unit, unit_label = input$unit_selector)
    })
    
    process_plot_obj <- reactive({
      req(analyzed_data(), input$unit_selector)
      df_unit <- analyzed_data() %>% 
        dplyr::filter(analysis_unit_id == input$unit_selector)
      
      # Calling the function defined in functions_plot.R
      plot_cusum_process_unit(df_unit, unit_label = input$unit_selector, h = input$param_h)
    })
    
    # ---------------------------------------------------------
    # 7. RENDERING & BUTTON CONTROL
    # ---------------------------------------------------------
    
    # IMPORTANT: Use parentheses () to "wake up" the reactive objects.
    output$plot_heatmap <- renderPlot({ heatmap_obj() })
    output$plot_series  <- renderPlot({ series_plot_obj() })
    output$plot_cusum_process <- renderPlot({ process_plot_obj() })
    
    # Enable Download Buttons only when data is ready
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
        paste0("CUSUM_Detailed_", clean_name, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        # Grab the already-calculated reactive plot
        ggplot2::ggsave(file, plot = series_plot_obj(), 
                        device = "png", width = 16, height = 7, dpi = 300)
      }
    )
    
    output$download_process_plot <- downloadHandler(
      filename = function() {
        clean_name <- sub(".*\\|", "", input$unit_selector)
        paste0("CUSUM_Process_", clean_name, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = process_plot_obj(), 
                        device = "png", width = 16, height = 7, dpi = 300)
      }
    )
    
    output$table_preview <- renderTable({ req(analyzed_data()); analyzed_data() })
  })
}