#' Register CUSUM module logic: tables.R
#'
#' @param state Internal module environment created by `cusumServer()`.
#' @return Invisibly returns `state`.
cusum_server_tables <- function(state) {
  evalq({
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
