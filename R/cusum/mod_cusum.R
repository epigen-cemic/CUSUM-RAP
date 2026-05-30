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
    ),
    
    # --- TIER 1: TOP NAVIGATION BAR ---
    div(class = "nav-bar",
        div(class = "nav-content",
            div(class = "header-logo-left") # Logo stays here
        )
    ),
    
    # --- TIER 2: HERO SECTION (Title Only) ---
    div(class = "hero-section",
        div(class = "hero-content",
            h1(class = "app-title", "CUSUM Analysis")
        )
    ),
    # --- MAIN CONTENT WRAPPER ---
    div(class = "content-wrapper",
        div(class = "well-custom",
            fluidRow(
              # SIDEBAR
              column(3,
                     div(class = "sidebar-dark",
                         fileInput(
                           ns("file_upload"),
                           "Upload API-Pop CSV",
                           multiple = TRUE,
                           accept = c(".csv")
                         ),
                         uiOutput(ns("uploaded_files_ui")),
                         uiOutput(ns("country_selector_ui")),
                         tags$label(class = "control-label", "Geographic Level"),
                         selectInput(ns("geo_level"), label = NULL, choices = NULL, selected = NULL, width = "100%"),
                         uiOutput(ns("dynamic_target_ui")),
                         br(),
                         tags$label(class = "control-label", "Baseline Configuration"),
                         numericInput(ns("param_weeks"), "Detection Period (Weeks):", 
                                      value = as.integer(cusum_config_value("default_detection_period", 52)), min = 4, max = 260, width = "100%"),
                         br(),
                         tags$label(class = "title-label", "Variables"),
                         br(),
                         tags$label(class = "control-label", "Expected Frequency (\u03BC0)"),
                         radioButtons(ns("mu_method"), label = NULL, 
                                      choices = c("Automatic (Poisson GLM)" = "auto", "Manual Input" = "manual")),
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
                         div(class = "variable-label", tags$label(class = "control-label", "Target ARL0 :"), 
                             numericInput(ns("param_arl0"), label = NULL, value = 100, min = 1)),
                         div(style = "min-height: 25px;"),
                         div(class = "parameter-block",
                             div(class = "variable-label", tags$label(class = "control-label", "RR (Relative Risk):"), 
                                 numericInput(ns("param_rr"), label = NULL, value = 2, min = 1.01, step = 0.01)),
                             uiOutput(ns("calc_k_text"))
                         ),
                         div(class = "parameter-block",
                             div(class = "variable-label", tags$label(class = "control-label", "h (Threshold):"), 
                                 numericInput(ns("param_h"), label = NULL, value = 5, min = 0.001, step = 0.001)),
                             uiOutput(ns("rec_text_h"))
                         ),
                         br(),
                         actionButton(ns("run_analysis"), "Run Analysis", class = "dashboard-run-button")
                     )
              ), # Close column 3
              
              # MAIN PANEL
              column(9,
                     uiOutput(ns("config_warning")),
                     uiOutput(ns("error_message")),
                     div(class = "main-tabs",
                         tabsetPanel(
                           tabPanel("Overview",
                                     br(),
                                     tags$p(class = "output-description",
                                            "Alerts are early warnings and require further epidemiological review. The detection period controls the recent weeks monitored for alarms."),
                                     uiOutput(ns("plot_heatmap_ui"))),
                           tabPanel("Detailed View", 
                                     br(),
                                     tags$p(class = "output-description",
                                            "Use this view to compare observed values, expected baseline values, and the CUSUM process for a selected location."),
                                     div(class = "detail-view-toolbar",
                                         div(class = "detail-view-downloads",
                                             shinyjs::disabled(downloadButton(ns("download_series_plot"), "Download Bar Plot", class = "btn-info")),
                                             shinyjs::disabled(downloadButton(ns("download_process_plot"), "Download Trends", class = "btn-warning"))
                                         )
                                     ),
                                     div(class = "detail-view-selectors-row",
                                         uiOutput(ns("unit_navigation_ui"))
                                     ),
                                    uiOutput(ns("selection_summary_ui")),
                                    br(),
                                    div(class = "detail-plot-card detail-plot-card-first",
                                        uiOutput(ns("plot_series_ui"))
                                    ),
                                    div(class = "detail-plot-divider"),
                                    div(class = "detail-plot-card",
                                        uiOutput(ns("plot_cusum_process_ui"))
                                    )
                           ),
                           tabPanel("Analysis Results", 
                                     br(),
                                     shinyjs::disabled(
                                       downloadButton(ns("download_data"), "Download Results CSV", class = "btn-info")
                                     ),
                                     br(), br(),
                                     DT::DTOutput(ns("table_preview"))
                            ),
                           tabPanel("Prepared Data",
                                    div(class = "prepared-panel",
                                        tags$p(tags$strong("Prepared Dataset Preview")),
                                        tags$p("This table shows the cleaned and aggregated dataset used for the CUSUM analysis."),
                                        tags$p(class = "prepared-note",
                                               "Data includes resolved overlaps, selected geographic level, and completed time series.")
                                    ),
                                    textOutput(ns("rows_added_info")),
                                     uiOutput(ns("coverage_status_ui")),
                                     uiOutput(ns("prepared_change_log")),
                                     uiOutput(ns("prepared_summary")),
                                     br(),
                                     shinyjs::disabled(downloadButton(ns("download_prepared_data"), "Download Prepared Data CSV", class = "btn-default")),
                                     br(), br(),
                                     DT::DTOutput(ns("prepared_data_table"))
                           ),
                           tabPanel("Help",
                                    br(),
                                    cusum_help_tab(ns)
                           )
                         )
                     )
              ) # Close column 9
            ) # Close fluidRow
        ) # Close well-custom
    ) # Close content-wrapper
  ) # Close fluidPage
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

    output$config_warning <- renderUI({
      if (is.null(cusum_config_messages) || length(cusum_config_messages) == 0) {
        return(NULL)
      }

      div(
        class = "dashboard-error",
        tags$strong("Configuration warning"),
        tags$ul(lapply(cusum_config_messages, tags$li))
      )
    })

    ns <- session$ns

    current_font_size_key <- reactive({
      value <- input$font_size
      if (is.null(value) || !nzchar(value)) "small" else value
    })

    display_digits <- reactive({
      value <- suppressWarnings(as.integer(input$display_digits))
      if (is.na(value)) 4L else max(0L, min(8L, value))
    })

    plot_font_size <- reactive({
      switch(current_font_size_key(),
             "small" = 18,
             "medium" = 20,
             "large" = 22,
             "xlarge" = 24,
             18)
    })


    plot_height_px <- reactive({
      # Keep the detailed plots wide instead of square.
      # Font size increases get extra vertical room, but not enough to push
      # the plots outside their cards or overlap the following plot.
      switch(current_font_size_key(),
             "small" = 500,
             "medium" = 560,
             "large" = 630,
             "xlarge" = 700,
             500)
    })

    heatmap_height_px <- reactive({
      switch(current_font_size_key(),
             "small" = 600,
             "medium" = 680,
             "large" = 760,
             "xlarge" = 840,
             600)
    })

    download_plot_width_in <- reactive({
      switch(current_font_size_key(),
             "small" = 16,
             "medium" = 17,
             "large" = 18,
             "xlarge" = 19,
             16)
    })

    download_plot_height_in <- reactive({
      # Preserve a wide 16:9-style aspect ratio in exported PNGs.
      download_plot_width_in() * 9 / 16
    })

    observe({
      size_class <- paste0("font-size-", current_font_size_key())
      shinyjs::runjs(sprintf(
        "document.body.classList.remove('font-size-small','font-size-medium','font-size-large','font-size-xlarge'); document.body.classList.add('%s');",
        size_class
      ))
    })

    detail_select_width <- function(choices, label = NULL) {
      choices <- as.character(choices)
      choices <- choices[!is.na(choices)]
      max_chars <- 0
      if (length(choices) > 0) {
        max_chars <- max(nchar(choices, type = "width"), na.rm = TRUE)
      }
      if (!is.null(label) && nzchar(label)) {
        max_chars <- max(max_chars, nchar(label, type = "width"))
      }

      char_px <- switch(current_font_size_key(),
                        "small" = 7.2,
                        "medium" = 8.0,
                        "large" = 8.8,
                        "xlarge" = 9.6,
                        7.2)
      min_px <- switch(current_font_size_key(),
                       "small" = 180,
                       "medium" = 205,
                       "large" = 230,
                       "xlarge" = 255,
                       180)

      # Extra room accounts for left/right padding and the select arrow.
      width_px <- ceiling(max(min_px, min(620, (max_chars * char_px) + 72)))
      paste0(width_px, "px")
    }
    
# ---------------------------------------------------------
    # 0. INITIALIZATION
    # ---------------------------------------------------------
    # Pull country options from config.json. If config$active_country contains
    # more than one valid country, the UI exposes a country selector.
    country_choices <- active_countries
    current_country <- reactive({
      if (length(country_choices) > 1) {
        req(input$active_country)
        input$active_country
      } else {
        active_country
      }
    })

    hierarchy_levels_list <- reactive({
      config[[current_country()]]$levels
    })

    overlap_preference <- reactiveVal(NULL)


    uploaded_file_state <- reactiveVal(NULL)

    format_uploaded_file_size <- function(bytes) {
      if (is.null(bytes) || is.na(bytes)) return("")
      if (bytes < 1024) return(paste0(bytes, " B"))
      if (bytes < 1024^2) return(paste0(round(bytes / 1024, 1), " KB"))
      paste0(round(bytes / 1024^2, 1), " MB")
    }

    active_uploaded_files <- reactive({
      files <- uploaded_file_state()
      req(!is.null(files), nrow(files) > 0)
      files <- files[isTRUE(files$active) | files$active, , drop = FALSE]
      req(nrow(files) > 0)
      files
    })

    active_uploaded_file_paths <- reactive({
      files <- active_uploaded_files()
      setNames(files$datapath, seq_along(files$datapath))
    })

    show_overlap_modal_if_needed <- function() {
      files <- uploaded_file_state()
      active_count <- if (is.null(files)) 0 else sum(files$active, na.rm = TRUE)

      if (active_count > 1) {
        overlap_preference(NULL)
        showModal(modalDialog(
          title = "Multiple Files Detected",
          p("How should we resolve overlapping location/week data?"),
          radioButtons(
            ns("modal_overlap_choice"),
            label = NULL,
            choices = c(
              "Keep Newest File Information" = "new",
              "Keep Oldest File Information" = "old",
              "Add Together (Sum)" = "sum"
            ),
            selected = "new"
          ),
          footer = tagList(
            actionButton(ns("confirm_overlap"), "Confirm & Continue", class = "btn-primary")
          ),
          easyClose = FALSE
        ))
      } else if (active_count == 1) {
        overlap_preference("sum")
      } else {
        overlap_preference(NULL)
      }
    }

    output$country_selector_ui <- renderUI({
      if (length(country_choices) <= 1) {
        return(NULL)
      }

      tagList(
        tags$label(class = "control-label", "Country"),
        selectInput(
          ns("active_country"),
          label = NULL,
          choices = country_choices,
          selected = active_country,
          width = "100%"
        )
      )
    })

    observeEvent(current_country(), {
      choices <- rap_cusum_geo_choices(config, current_country())
      selected <- if (length(choices) >= 2) unname(choices)[2] else unname(choices)[1]

      updateSelectInput(
        session,
        "geo_level",
        choices = choices,
        selected = selected
      )
    }, ignoreInit = FALSE)
    
    # ---------------------------------------------------------
    # 1. RAW DATA PROCESSING & CONFLICT RESOLUTION
    # ---------------------------------------------------------
    raw_combined_data <- reactive({
      req(active_uploaded_file_paths())
      tryCatch({
        raw_df <- api_pop_combine_files(
          file_paths = active_uploaded_file_paths(),
          config = config,
          active_country = current_country(),
          require_population = FALSE
        )

        validation_msg <- validate_cusum_input(raw_df)
        if (!is.null(validation_msg)) {
          stop(validation_msg)
        }

        return(raw_df)
      }, error = function(e) {
        showNotification(paste("Error reading files:", e$message), type = "error")
        return(NULL)
      })
    })

    observeEvent(input$file_upload, {
      req(input$file_upload)

      uploaded <- input$file_upload
      new_files <- data.frame(
        id = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", seq_len(nrow(uploaded)), "_", make.names(uploaded$name)),
        name = uploaded$name,
        datapath = uploaded$datapath,
        size = uploaded$size,
        uploaded_at = as.character(Sys.time()),
        active = TRUE,
        stringsAsFactors = FALSE
      )

      existing <- uploaded_file_state()
      if (is.null(existing)) {
        uploaded_file_state(new_files)
      } else {
        uploaded_file_state(rbind(existing, new_files))
      }

      show_overlap_modal_if_needed()
    }, ignoreInit = TRUE)
    
    observeEvent(input$confirm_overlap, {
      overlap_preference(input$modal_overlap_choice)
      removeModal()
    })


    output$uploaded_files_ui <- renderUI({
      files <- uploaded_file_state()
      uploaded_count <- if (is.null(files)) 0 else nrow(files)
      active_count <- if (is.null(files)) 0 else sum(files$active, na.rm = TRUE)

      tagList(
        div(
          class = "uploaded-files-summary",
          div(
            class = "uploaded-files-summary-row",
            span(class = "uploaded-files-label", "Uploaded files"),
            span(class = "uploaded-files-count", uploaded_count)
          ),
          div(
            class = "uploaded-files-summary-row",
            span(class = "uploaded-files-label muted", "Active files"),
            span(class = "uploaded-files-count active", active_count)
          )
        ),
        if (!is.null(files) && nrow(files) > 0) {
          actionButton(ns("manage_uploaded_files"), "Manage files", class = "dashboard-small-button")
        }
      )
    })

    observeEvent(input$manage_uploaded_files, {
      files <- uploaded_file_state()

      if (is.null(files) || nrow(files) == 0) {
        showModal(modalDialog(
          title = "Uploaded files",
          p("No files have been uploaded."),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }

      table_rows <- lapply(seq_len(nrow(files)), function(i) {
        tags$tr(
          tags$td(
            checkboxInput(
              ns(paste0("file_active_", files$id[i])),
              label = NULL,
              value = isTRUE(files$active[i]),
              width = "20px"
            )
          ),
          tags$td(files$name[i]),
          tags$td(format_uploaded_file_size(files$size[i]))
        )
      })

      showModal(modalDialog(
        title = "Uploaded files",
        size = "l",
        easyClose = TRUE,
        tags$p("Uncheck files that should not be included in the analysis. Use 'Remove inactive files' to remove unchecked files from this session."),
        tags$table(
          class = "uploaded-files-table",
          tags$thead(tags$tr(tags$th("Active"), tags$th("File name"), tags$th("Size"))),
          tags$tbody(table_rows)
        ),
        footer = tagList(
          actionButton(ns("apply_file_selection"), "Apply selection", class = "dashboard-primary-button"),
          actionButton(ns("remove_inactive_files"), "Remove inactive files", class = "dashboard-danger-button"),
          actionButton(ns("clear_all_files_modal"), "Clear all files", class = "dashboard-danger-button"),
          modalButton("Close")
        )
      ))
    })

    observeEvent(input$apply_file_selection, {
      files <- uploaded_file_state()
      req(!is.null(files), nrow(files) > 0)

      files$active <- vapply(
        files$id,
        function(id) isTRUE(input[[paste0("file_active_", id)]]),
        logical(1)
      )

      uploaded_file_state(files)
      removeModal()
      show_overlap_modal_if_needed()
      showNotification(paste(sum(files$active), "active file(s) selected."), type = "message")
    })

    observeEvent(input$remove_inactive_files, {
      files <- uploaded_file_state()
      req(!is.null(files), nrow(files) > 0)

      files$active <- vapply(
        files$id,
        function(id) isTRUE(input[[paste0("file_active_", id)]]),
        logical(1)
      )
      files <- files[files$active, , drop = FALSE]

      if (nrow(files) == 0) {
        uploaded_file_state(NULL)
      } else {
        uploaded_file_state(files)
      }

      removeModal()
      show_overlap_modal_if_needed()
      showNotification("Inactive files were removed.", type = "message")
    })

    observeEvent(input$clear_all_files_modal, {
      uploaded_file_state(NULL)
      overlap_preference(NULL)
      removeModal()
      showNotification("All uploaded files were removed.", type = "message")
    })


    output$error_message <- renderUI({
      if (is.null(uploaded_file_state())) return(NULL)

      df <- tryCatch(raw_combined_data(), error = function(e) {
        div(
          class = "dashboard-error",
          paste("Upload validation error:", e$message)
        )
      })

      if (inherits(df, "shiny.tag")) return(df)
      NULL
    })
    
    # ---------------------------------------------------------
    # 2. DYNAMIC UI & TARGET LOCATIONS
    # ---------------------------------------------------------
    output$dynamic_target_ui <- renderUI({
      req(raw_combined_data(), input$geo_level)
      
      raw_df <- raw_combined_data()
      h_levels <- hierarchy_levels_list()
      generic_map <- c("country", "level1", "level2", "level3", "level4")
      
      target_col <- generic_map[which(h_levels == input$geo_level)]

      locs <- NULL
      if (length(target_col) > 0 && target_col %in% names(raw_df)) {
        locs <- unique(raw_df[[target_col]])
        locs <- sort(locs[!is.na(locs) & locs != ""])
      }
      
      tagList(
        selectizeInput(ns("target_locations"), "Locations:",
                       choices = locs,
                       selected = character(0),
                       multiple = TRUE,
                       options = list(placeholder = 'Leave empty to include all locations',
                                      plugins = list('remove_button')),
                       width = "100%"),
        tags$small(style = "color:#cccccc; display:block; margin-top:-8px;",
                   "Leave empty to include all available locations.")
      )
    })
    
    # ---------------------------------------------------------
    # 3. DATA PREPARATION (The Gatekeeper)
    # ---------------------------------------------------------
        prepared_target_data <- reactive({
      req(raw_combined_data(), input$geo_level, overlap_preference())

      raw_df <- raw_combined_data()
      h_levels <- hierarchy_levels_list()
      generic_map <- c("country", "level1", "level2", "level3", "level4")
      level_depth <- which(h_levels == input$geo_level)

      if (length(level_depth) == 0 || is.na(level_depth)) {
        return(NULL)
      }

      target_locations <- input$target_locations

      process_target_data(
        raw_df           = raw_df,
        target_locations = target_locations,
        target_col       = generic_map[level_depth],
        req_cols         = generic_map[1:level_depth],
        overlap_method   = overlap_preference(),
        hierarchy_levels = h_levels,
        selected_level   = input$geo_level
      )
    })
    
    # ---------------------------------------------------------
    # 4. SMART PARAMETER RECOMMENDATIONS & CALCULATIONS
    # ---------------------------------------------------------
    current_k <- reactive({
      req(input$param_rr)

      if (input$mu_method == "manual") {
        req(input$param_mu)
        mu_val <- input$param_mu
      } else {
        # Automatic method: needs a dataset to calculate baseline.
        prepared_df <- prepared_target_data()
        req(prepared_df)

        coverage_validation <- cusum_assess_prepared_coverage(
          prepared_df,
          detection_period = as.numeric(input$param_weeks)
        )

        if (identical(coverage_validation$status, "stop")) {
          return(NULL)
        }

        mu_val <- get_phase1_baseline(prepared_df, as.numeric(input$param_weeks))
      }
      k_val <- calculate_k_from_rr(input$param_rr, mu_val)
      return(k_val)
    })
    
    output$calc_k_text <- renderUI({
      k_val <- current_k()
      if (!is.null(k_val)) {
        tags$small(class = "parameter-helper-text", 
                   paste("Calculated k:", round(k_val, 3)))
      } else {
        tags$small(class = "parameter-helper-text parameter-helper-muted", 
                   "k will be calculated automatically.")
      }
    })
    
    output$rec_text_h <- renderUI({
      req(input$param_arl0, current_k())
      
      rec_val <- recommend_h(input$param_arl0, current_k())
      
      if (!is.na(rec_val)) {
        tags$small(
          class = "parameter-helper-text", 
          paste("Recommended h:", rec_val)
        )
      } else {
        tags$small(
          class = "parameter-helper-text parameter-helper-error", 
          "Unable to calculate h recommendation."
        )
      }
    })

    # ---------------------------------------------------------
    # 5. CUSUM MATH ENGINE
    # ---------------------------------------------------------
    analyzed_data <- eventReactive(input$run_analysis, {
      req(prepared_target_data(), input$param_h, input$param_rr, input$param_weeks)
      
      prepared_df <- prepared_target_data() 
      if (nrow(prepared_df) == 0) return(NULL)
      
      window_size <- as.numeric(input$param_weeks)
      coverage_validation <- cusum_assess_prepared_coverage(
        prepared_df,
        detection_period = window_size
      )

      if (identical(coverage_validation$status, "stop")) {
        shiny::validate(shiny::need(FALSE, coverage_validation$message))
      }

      if (identical(coverage_validation$status, "warn")) {
        showNotification(coverage_validation$message, type = "warning", duration = 12)
      }

      max_week_index <- max(prepared_df$time_index, na.rm = TRUE)
      
      if (max_week_index < window_size) {
        shiny::validate(shiny::need(max_week_index >= window_size,
            sprintf("Error: You requested %d weeks, but only %d are available.",
                    window_size, max_week_index)
            )
        )
      }
      
      start_week <- max(0, max_week_index - window_size)
      
      if (input$mu_method == "manual") {
        req(input$param_mu)
        final_mu <- input$param_mu
      } else {
        final_mu <- get_phase1_baseline(prepared_df, window_size)
      }
      
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
      
      if (length(calculated_k) == 1) {
        res$k_value <- calculated_k
      } else if (is.vector(calculated_k) && !is.null(names(calculated_k))) {
        res$k_value <- calculated_k[res$analysis_unit_id]
      } else {
        res$k_value <- calculated_k[1]
      }
      
      return(res)
    })
    
    detail_level_depth <- reactive({
      req(input$geo_level)
      h_levels <- hierarchy_levels_list()
      level_depth <- which(h_levels == input$geo_level)
      if (length(level_depth) == 0 || is.na(level_depth)) return(NULL)
      level_depth
    })

    detail_geo_columns <- reactive({
      req(detail_level_depth())
      generic_map <- c("country", "level1", "level2", "level3", "level4")
      generic_map[seq_len(detail_level_depth())]
    })

    detail_geo_labels <- reactive({
      req(detail_level_depth())
      labels <- hierarchy_levels_list()[seq_len(detail_level_depth())]
      tools::toTitleCase(labels)
    })

    output$unit_navigation_ui <- renderUI({
      req(analyzed_data(), detail_geo_columns(), detail_geo_labels())

      df <- analyzed_data()
      cols <- detail_geo_columns()
      labels <- detail_geo_labels()
      cols <- cols[cols %in% names(df)]
      labels <- labels[seq_along(cols)]

      if (length(cols) == 0) return(NULL)

      selectors <- list()
      filtered_df <- df

      for (i in seq_along(cols)) {
        col <- cols[i]
        input_id <- paste0("detail_", col)
        choices <- sort(unique(as.character(filtered_df[[col]])))
        choices <- choices[!is.na(choices) & choices != ""]
        if (length(choices) == 0) next

        current_value <- input[[input_id]]
        selected_value <- if (!is.null(current_value) && current_value %in% choices) current_value else choices[1]

        selectors[[length(selectors) + 1]] <- div(
          class = "detail-location-selector",
          selectizeInput(
            ns(input_id),
            labels[i],
            choices = choices,
            selected = selected_value,
            width = detail_select_width(choices, labels[i]),
            options = list(
              dropdownParent = "body",
              maxOptions = 10000
            )
          )
        )

        filtered_df <- filtered_df %>%
          dplyr::filter(as.character(.data[[col]]) == selected_value)
      }

      tagList(
        tags$label(class = "control-label", "Select Location"),
        div(class = "detail-location-selectors", selectors)
      )
    })

    selected_unit_id <- reactive({
      req(analyzed_data(), detail_geo_columns())

      df <- analyzed_data()
      cols <- detail_geo_columns()
      cols <- cols[cols %in% names(df)]
      if (length(cols) == 0) return(NULL)

      filtered_df <- df
      for (col in cols) {
        input_id <- paste0("detail_", col)
        choices <- sort(unique(as.character(filtered_df[[col]])))
        choices <- choices[!is.na(choices) & choices != ""]
        if (length(choices) == 0) return(NULL)

        selected_value <- input[[input_id]]
        if (is.null(selected_value) || !(selected_value %in% choices)) {
          selected_value <- choices[1]
        }

        filtered_df <- filtered_df %>%
          dplyr::filter(as.character(.data[[col]]) == selected_value)
      }

      units <- unique(filtered_df$analysis_unit_id)
      if (length(units) == 0) return(NULL)
      units[1]
    })

    format_summary_value <- function(value, digits = NULL) {
      if (is.null(value) || length(value) == 0 || all(is.na(value))) return("Not available")
      value <- value[!is.na(value)][1]
      if (is.numeric(value)) {
        if (!is.null(digits)) return(format(round(value, digits), big.mark = ",", trim = TRUE, scientific = FALSE))
        return(format(value, big.mark = ",", trim = TRUE, scientific = FALSE))
      }
      as.character(value)
    }

    output$selection_summary_ui <- renderUI({
      req(analyzed_data(), selected_unit_id())

      df_unit <- analyzed_data() %>%
        dplyr::filter(analysis_unit_id == selected_unit_id())
      req(nrow(df_unit) > 0)

      cols <- detail_geo_columns()
      labels <- detail_geo_labels()
      cols <- cols[cols %in% names(df_unit)]
      labels <- labels[seq_along(cols)]

      location_items <- lapply(seq_along(cols), function(i) {
        current_col <- cols[[i]]
        current_label <- labels[[i]]
        current_value <- unique(df_unit[[current_col]])
        formatted_value <- format_summary_value(current_value)

        label_element <- span(
          class = "selection-summary-label",
          paste0(current_label, ":")
        )

        value_element <- span(
          class = "selection-summary-value",
          formatted_value
        )

        div(
          class = "selection-summary-item",
          label_element,
          value_element
        )
      })

      date_range <- "Not available"
      weeks_analyzed <- "Not available"
      if ("epi_date" %in% names(df_unit)) {
        date_range <- paste0(
          format(min(as.Date(df_unit$epi_date), na.rm = TRUE), "%G-W%V"),
          " to ",
          format(max(as.Date(df_unit$epi_date), na.rm = TRUE), "%G-W%V")
        )
        weeks_analyzed <- format_summary_value(dplyr::n_distinct(df_unit$epi_date))
      }

      population_label <- "Not available"
      if ("population" %in% names(df_unit)) {
        valid_pop <- df_unit$population[!is.na(df_unit$population) & df_unit$population > 0]
        if (length(valid_pop) > 0) {
          population_label <- format(round(stats::median(valid_pop, na.rm = TRUE)), big.mark = ",", trim = TRUE, scientific = FALSE)
        }
      }

      alarm_label <- "No"
      latest_alarm_label <- "None"
      if ("alarm" %in% names(df_unit) && any(df_unit$alarm, na.rm = TRUE)) {
        alarm_label <- "Yes"
        alarm_rows <- df_unit[df_unit$alarm %in% TRUE, , drop = FALSE]
        if ("epi_date" %in% names(alarm_rows) && nrow(alarm_rows) > 0) {
          latest_alarm_label <- format(max(as.Date(alarm_rows$epi_date), na.rm = TRUE), "%G-W%V")
        }
      }

      tagList(
        div(class = "selection-summary-card",
            div(class = "selection-summary-title", "Current selection summary"),
            div(class = "selection-summary-grid",
                location_items,
                div(class = "selection-summary-item",
                    span(class = "selection-summary-label", "Analysis level:"),
                    span(class = "selection-summary-value", format_summary_value(input$geo_level))
                ),
                div(class = "selection-summary-item",
                    span(class = "selection-summary-label", "Weeks analyzed:"),
                    span(class = "selection-summary-value", weeks_analyzed)
                ),
                div(class = "selection-summary-item",
                    span(class = "selection-summary-label", "Date range:"),
                    span(class = "selection-summary-value", date_range)
                ),
                div(class = "selection-summary-item",
                    span(class = "selection-summary-label", "Population:"),
                    span(class = "selection-summary-value", population_label)
                ),
                div(class = "selection-summary-item",
                    span(class = "selection-summary-label", "Alarms detected:"),
                    span(class = "selection-summary-value", alarm_label)
                ),
                div(class = "selection-summary-item",
                    span(class = "selection-summary-label", "Most recent alarm:"),
                    span(class = "selection-summary-value", latest_alarm_label)
                )
            )
        )
      )
    })
    
    # ---------------------------------------------------------
    # 6. REACTIVE PLOT OBJECTS (The Logic Hub)
    # ---------------------------------------------------------
    heatmap_obj <- reactive({
      req(analyzed_data())
      plot_cusum_alarms_overview(analyzed_data(), base_size = plot_font_size())
    })
    
    series_plot_obj <- reactive({
      req(analyzed_data(), selected_unit_id())
      df_unit <- analyzed_data() %>% 
        dplyr::filter(analysis_unit_id == selected_unit_id())
      
      plot_cusum_series_unit(df_unit, unit_label = selected_unit_id(), base_size = plot_font_size())
    })
    
    process_plot_obj <- reactive({
      req(analyzed_data(), selected_unit_id())
      df_unit <- analyzed_data() %>% 
        dplyr::filter(analysis_unit_id == selected_unit_id())
      
      unit_k <- unique(df_unit$k_value)[1]
      unit_rate <- NA_real_
      if (all(c("mu_hat", "population") %in% names(df_unit))) {
        valid_pop <- !is.na(df_unit$population) & df_unit$population > 0
        if (any(valid_pop)) {
          unit_rate <- mean((df_unit$mu_hat[valid_pop] / df_unit$population[valid_pop]) * 100000, na.rm = TRUE)
        }
      }
      
      plot_cusum_process_unit(df_unit, 
                              unit_label = selected_unit_id(), 
                              h = input$param_h,
                              k = unit_k,
                              arl0 = input$param_arl0,
                              rr = input$param_rr,
                              rate_per_100k = unit_rate,
                              base_size = plot_font_size())
    })
    
    # ---------------------------------------------------------
    # 7. RENDERING & BUTTON CONTROL
    # ---------------------------------------------------------
    output$plot_heatmap_ui <- renderUI({
      plotOutput(ns("plot_heatmap"), height = paste0(heatmap_height_px(), "px"))
    })

    output$plot_series_ui <- renderUI({
      plotOutput(ns("plot_series"), height = paste0(plot_height_px(), "px"))
    })

    output$plot_cusum_process_ui <- renderUI({
      plotOutput(ns("plot_cusum_process"), height = paste0(plot_height_px(), "px"))
    })

    output$plot_heatmap <- renderPlot({ heatmap_obj() }, height = function() heatmap_height_px())
    output$plot_series  <- renderPlot({ series_plot_obj() }, height = function() plot_height_px())
    output$plot_cusum_process <- renderPlot({ process_plot_obj() }, height = function() plot_height_px())
    
    output$table_preview <- DT::renderDT({
      req(analyzed_data())
      
      df <- analyzed_data()
      
      if ("epi_date" %in% names(df)) {
        df$epi_date <- format(as.Date(df$epi_date), "%G-W%V")
      }
      
      dt <- DT::datatable(df, options = list(
          pageLength = 25, scrollX = TRUE,
          columnDefs = list(list(className = 'dt-left', targets = "_all")),
          order = list(list(1, 'desc'))),
        rownames = FALSE)

      numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      decimal_cols <- numeric_cols[vapply(numeric_cols, function(col) {
        values <- df[[col]]
        values <- values[!is.na(values)]
        if (length(values) == 0) return(FALSE)
        any(abs(values - round(values)) > .Machine$double.eps^0.5)
      }, logical(1))]

      if (length(decimal_cols) > 0) {
        dt <- DT::formatRound(dt, columns = decimal_cols, digits = display_digits())
      }

      dt
    })
    
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
    

    output$rows_added_info <- renderText({
      df <- prepared_target_data()
      if (is.null(df)) return("")

      coverage <- attr(df, "cusum_coverage_summary", exact = TRUE)
      if (is.null(coverage) || nrow(coverage) == 0) return("")

      sprintf(
        "Missing weeks filled with 0 cases: %s. Observed weeks before filling: %s. Prepared weeks after filling: %s.",
        sum(coverage$missing_weeks, na.rm = TRUE),
        sum(coverage$observed_weeks, na.rm = TRUE),
        sum(coverage$prepared_weeks, na.rm = TRUE)
      )
    })


    output$coverage_status_ui <- renderUI({
      df <- prepared_target_data()
      if (is.null(df)) return(NULL)

      validation <- cusum_assess_prepared_coverage(
        df,
        detection_period = as.numeric(input$param_weeks)
      )

      css_class <- switch(
        validation$status,
        stop = "dashboard-error",
        warn = "prepared-warning",
        ok = "prepared-ok",
        "prepared-panel"
      )

      div(
        class = css_class,
        tags$strong("Coverage assessment: "),
        validation$message
      )
    })


    output$prepared_change_log <- renderUI({
      df <- prepared_target_data()
      if (is.null(df)) return(NULL)
      log_items <- api_pop_get_log(df)
      if (length(log_items) == 0) return(NULL)

      div(
        class = "prepared-log",
        h4("Data preparation log"),
        tags$ul(lapply(log_items, tags$li))
      )
    })

    output$prepared_summary <- renderUI({
      
      df <- prepared_target_data()
      
      if (is.null(df)) {
        return(
          div(class = "prepared-panel",
              tags$p("Upload data and select locations to preview dataset."))
          )
      }
      
      if (nrow(df) == 0) {
        return(
          div(class = "prepared-panel",
              tags$p("No prepared data available."))
        )
      }
      
      weeks <- length(unique(df$time_index))
      
      h_levels <- hierarchy_levels_list()
      generic_map <- c("country", "level1", "level2", "level3", "level4")
      
      available_levels <- generic_map[generic_map %in% names(df)]
      level_names <- h_levels[seq_along(available_levels)]
      
      counts <- lapply(seq_along(available_levels), function(i) {
        col <- available_levels[i]
        n <- length(unique(df[[col]][!is.na(df[[col]]) & df[[col]] != ""]))
        list(label = level_names[i], value = n)
      })
      
      coverage <- attr(df, "cusum_coverage_summary", exact = TRUE)
      if (!is.null(coverage) && nrow(coverage) > 0) {
        counts <- c(
          counts,
          list(
            list(label = "Prepared weeks", value = sum(coverage$prepared_weeks, na.rm = TRUE)),
            list(label = "Observed weeks", value = sum(coverage$observed_weeks, na.rm = TRUE)),
            list(label = "Filled zero weeks", value = sum(coverage$missing_weeks, na.rm = TRUE)),
            list(label = "Lowest coverage", value = cusum_percent(min(coverage$observed_coverage, na.rm = TRUE)))
          )
        )
      } else {
        counts <- c(counts, list(list(label = "Weeks", value = weeks)))
      }
      
      div(
        class = "prepared-panel",
        div(class = "summary-container",
          lapply(counts, function(x) {
            div(
              class = "summary-card",
              div(class = "summary-label", x$label),
              div(class = "summary-value", x$value)
            )
          })
        )
      )
    })
    


    output$prepared_data_table <- DT::renderDT({
      req(prepared_target_data())
      DT::datatable(
        prepared_target_data(),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
    })

    observe({
      if (isTruthy(prepared_target_data())) {
        shinyjs::enable("download_prepared_data")
      } else {
        shinyjs::disable("download_prepared_data")
      }
    })
    
    # ---------------------------------------------------------
    # 8. DOWNLOAD HANDLERS (Connected to Reactives)
    # ---------------------------------------------------------
    output$download_series_plot <- downloadHandler(
      filename = function() {
        clean_name <- sub(".*\\|", "", selected_unit_id())
        paste0("CUSUM_Bar_plot_", clean_name, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = series_plot_obj(), 
                        device = "png", width = download_plot_width_in(), height = download_plot_height_in(), dpi = 300)
      }
    )
    
    output$download_process_plot <- downloadHandler(
      filename = function() {
        clean_name <- sub(".*\\|", "", selected_unit_id())
        paste0("CUSUM_Trends_", clean_name, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = process_plot_obj(), 
                        device = "png", width = download_plot_width_in(), height = download_plot_height_in(), dpi = 300)
      }
    )
    
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
    
    output$download_prepared_data <- downloadHandler(
      filename = function() {
        paste0("CUSUM_Prepared_Data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- prepared_target_data()

        if ("epi_date" %in% names(df)) {
          df$epi_date <- format(as.Date(df$epi_date), "%G-W%V")
        }

        write.csv(df, file, row.names = FALSE)
      }
    )

  })
}