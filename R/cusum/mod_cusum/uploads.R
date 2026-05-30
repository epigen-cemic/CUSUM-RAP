#' Register CUSUM module logic: uploads.R
#'
#' @param state Internal module environment created by `cusumServer()`.
#' @return Invisibly returns `state`.
cusum_server_uploads <- function(state) {
  evalq({
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

  }, envir = state)
  invisible(state)
}
