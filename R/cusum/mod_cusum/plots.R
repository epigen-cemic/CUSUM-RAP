#' Register CUSUM module logic: plots.R
#'
#' @param state Internal module environment created by `cusumServer()`.
#' @return Invisibly returns `state`.
cusum_server_plots <- function(state) {
  evalq({
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
            final_mu <- get_phase1_baseline_by_unit(prepared_df, window_size)
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

          if (length(calculated_k) == 1 || is.null(names(calculated_k))) {
            res$k_value <- calculated_k[1]
          } else {
            res$k_value <- as.numeric(calculated_k[as.character(res$analysis_unit_id)])
          }

          return(res)
        })

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
  }, envir = state)
  invisible(state)
}
