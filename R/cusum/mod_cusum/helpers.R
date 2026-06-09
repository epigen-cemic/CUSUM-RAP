build_table_filter_ui <- function(ns, id_prefix, df) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  
  column_choices <- names(df)
  selected_col <- column_choices[[1]]
  
  div(
    class = "table-filter-bar",
    tags$label(
      class = "table-filter-inline-label",
      "Search by:"
    ),
    selectInput(
      ns(paste0(id_prefix, "_filter_col")),
      label = NULL,
      choices = column_choices,
      selected = selected_col,
      width = "220px"
    ),
    uiOutput(ns(paste0(id_prefix, "_filter_value_ui")), inline = TRUE)
  )
}

build_table_filter_value_ui <- function(ns, id_prefix, df, selected_col) {
  if (is.null(df) || is.null(selected_col) || !selected_col %in% names(df)) {
    return(NULL)
  }
  
  values <- df[[selected_col]]
  
  if (is.numeric(values)) {
    range_values <- range(values, na.rm = TRUE)
    
    if (!all(is.finite(range_values))) {
      return(NULL)
    }
    
    return(
      div(
        class = "table-filter-values",
        numericInput(
          ns(paste0(id_prefix, "_filter_min")),
          label = NULL,
          value = range_values[[1]],
          min = range_values[[1]],
          max = range_values[[2]],
          width = "130px"
        ),
        tags$span(class = "table-filter-separator", "to"),
        numericInput(
          ns(paste0(id_prefix, "_filter_max")),
          label = NULL,
          value = range_values[[2]],
          min = range_values[[1]],
          max = range_values[[2]],
          width = "130px"
        )
      )
    )
  }
  
  textInput(
    ns(paste0(id_prefix, "_filter_text")),
    label = NULL,
    value = "",
    placeholder = paste("Search", selected_col),
    width = "260px"
  )
}

apply_table_filter <- function(df, selected_col, text_value = NULL, min_value = NULL, max_value = NULL) {
  if (is.null(df) || is.null(selected_col) || !selected_col %in% names(df)) {
    return(df)
  }
  
  values <- df[[selected_col]]
  
  if (is.numeric(values)) {
    if (!is.null(min_value) && is.finite(min_value)) {
      df <- df[is.na(df[[selected_col]]) | df[[selected_col]] >= min_value, , drop = FALSE]
    }
    
    if (!is.null(max_value) && is.finite(max_value)) {
      df <- df[is.na(df[[selected_col]]) | df[[selected_col]] <= max_value, , drop = FALSE]
    }
    
    return(df)
  }
  
  if (is.null(text_value) || !nzchar(text_value)) {
    return(df)
  }
  
  keep <- grepl(text_value, as.character(values), ignore.case = TRUE, fixed = TRUE)
  df[keep, , drop = FALSE]
}

#' Register CUSUM module logic: helpers.R
#'
#' @param state Internal module environment created by `cusumServer()`.
#' @return Invisibly returns `state`.
cusum_server_helpers <- function(state) {
  evalq({
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

  }, envir = state)
  invisible(state)
}
