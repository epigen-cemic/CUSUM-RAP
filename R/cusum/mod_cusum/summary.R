#' Register CUSUM module logic: summary.R
#'
#' @param state Internal module environment created by `cusumServer()`.
#' @return Invisibly returns `state`.
cusum_server_summary <- function(state) {
  evalq({
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

  }, envir = state)
  invisible(state)
}
