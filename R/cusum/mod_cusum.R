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
                                    numericInput(ns("param_mu"), label = NULL, value = 10, min = 1, width = "100%"),
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
                                    tags$h4("Unit-level reference"),
                                    tags$p(class = "prepared-note", "Expected weekly cases and expected rates are shown once per analysis unit."),
                                    uiOutput(ns("reference_table_filter_ui")),
                                    DT::DTOutput(ns("table_reference")),
                                    br(),
                                    
                                    tags$h4("Weekly results"),
                                    uiOutput(ns("weekly_table_filter_ui")),
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
    state <- new.env(parent = environment())
    state$input <- input
    state$output <- output
    state$session <- session

    cusum_server_helpers(state)
    cusum_server_uploads(state)
    cusum_server_selectors(state)
    cusum_server_inputs(state)
    cusum_server_summary(state)
    cusum_server_plots(state)
    cusum_server_tables(state)
    cusum_server_downloads(state)

    invisible(NULL)
  })
}
