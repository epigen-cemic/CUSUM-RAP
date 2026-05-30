#' Register CUSUM module logic: selectors.R
#'
#' @param state Internal module environment created by `cusumServer()`.
#' @return Invisibly returns `state`.
cusum_server_selectors <- function(state) {
  evalq({
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

  }, envir = state)
  invisible(state)
}
