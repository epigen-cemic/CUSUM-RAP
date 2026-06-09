#' Register CUSUM module logic: tables.R
#'
#' @param state Internal module environment created by `cusumServer()`.
#' @return Invisibly returns `state`.
cusum_server_tables <- function(state) {
  evalq({
        weekly_table_data <- reactive({
          req(analyzed_data())
          
          df <- format_cusum_weekly_table(analyzed_data())
          
          selected_col <- input$weekly_filter_col
          
          if (is.null(selected_col) || !selected_col %in% names(df)) {
            return(df)
          }
          
          apply_table_filter(
            df,
            selected_col = selected_col,
            text_value = input$weekly_filter_text,
            min_value = suppressWarnings(as.numeric(input$weekly_filter_min)),
            max_value = suppressWarnings(as.numeric(input$weekly_filter_max))
          )
        })
    
        reference_table_data <- reactive({
          req(analyzed_data())
          
          df <- format_cusum_reference_table(analyzed_data())
          
          selected_col <- input$reference_filter_col
          
          if (is.null(selected_col) || !selected_col %in% names(df)) {
            return(df)
          }
          
          apply_table_filter(
            df,
            selected_col = selected_col,
            text_value = input$reference_filter_text,
            min_value = suppressWarnings(as.numeric(input$reference_filter_min)),
            max_value = suppressWarnings(as.numeric(input$reference_filter_max))
          )
        })
        
        output$weekly_table_filter_ui <- renderUI({
          req(analyzed_data())
          df <- format_cusum_weekly_table(analyzed_data())
          build_table_filter_ui(session$ns, "weekly", df)
        })
        
        output$weekly_filter_value_ui <- renderUI({
          req(analyzed_data())
          df <- format_cusum_weekly_table(analyzed_data())
          build_table_filter_value_ui(session$ns, "weekly", df, input$weekly_filter_col)
        })
        
        output$reference_table_filter_ui <- renderUI({
          req(analyzed_data())
          df <- format_cusum_reference_table(analyzed_data())
          build_table_filter_ui(session$ns, "reference", df)
        })
        
        output$reference_filter_value_ui <- renderUI({
          req(analyzed_data())
          df <- format_cusum_reference_table(analyzed_data())
          build_table_filter_value_ui(session$ns, "reference", df, input$reference_filter_col)
        })

        output$table_preview <- DT::renderDT({
          req(analyzed_data())
          
          df <- weekly_table_data()
          
          dt <- DT::datatable(
            df,
            extensions = "Buttons",
            options = list(
              pageLength = 25,
              scrollX = TRUE,
              searchHighlight = TRUE,
              dom = "Blrtip",
              buttons = list(
                list(
                  extend = "colvis",
                  text = "Show / hide columns"
                )
              ),
              columnDefs = list(
                list(className = "dt-left", targets = "_all")
              ),
              order = list(list(1, "desc"))
            ),
            rownames = FALSE
          )
          
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
        
        output$table_reference <- DT::renderDT({
          req(analyzed_data())

          df <- reference_table_data()

          dt <- DT::datatable(
            df,
            extensions = "Buttons",
            options = list(
              pageLength = 25,
              scrollX = TRUE,
              searchHighlight = TRUE,
              dom = "Bltip",
              buttons = list(
                list(
                  extend = "colvis",
                  text = "Show / hide columns"
                )
              ),
              columnDefs = list(
                list(className = "dt-left", targets = "_all")
              )
            ),
            rownames = FALSE
          )

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

  }, envir = state)
  invisible(state)
}
