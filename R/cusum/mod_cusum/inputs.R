#' Register CUSUM module logic: inputs.R
#'
#' @param state Internal module environment created by `cusumServer()`.
#' @return Invisibly returns `state`.
cusum_server_inputs <- function(state) {
  evalq({
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

            unit_mu <- get_phase1_baseline_by_unit(prepared_df, as.numeric(input$param_weeks))
            mu_val <- mean(unit_mu, na.rm = TRUE)
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
  }, envir = state)
  invisible(state)
}
