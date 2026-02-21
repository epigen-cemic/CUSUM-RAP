## -----------------------------------------------------------
## functions_plot.R
## Plotting functions for CUSUM with Smart X-Axis and Clean IDs
## -----------------------------------------------------------

#' @title Smart X-Axis Labeler for Epidemiological Timelines
#'
#' @description
#' Generates labels for a continuous time index. Displays week numbers every 
#' 5 weeks. If a year change occurs (week resets to 1) or it is the start of 
#' the series, it explicitly displays the Year in brackets on a new line.
#'
#' @param breaks Numeric vector of breaks calculated by ggplot.
#' @param df The data frame used in the plot, containing 'week', 'year', and 'time_index'.
#'
#' @return A character vector of labels with vertical stacking for years.
smart_labs <- function(breaks, df) {
  sapply(breaks, function(b) {
    # Find the row corresponding to this time_index
    row <- df[df$time_index == round(b), ]
    if (nrow(row) == 0) return("")
    
    curr_week <- row$week[1]
    curr_year <- row$year[1]
    
    # Check for Year Transition: previous week was higher or current week is 1
    # or it is the absolute start of the graphic
    prev_row <- df[df$time_index == round(b - 1), ]
    is_year_start <- (nrow(prev_row) > 0 && prev_row$year[1] < curr_year) || 
      curr_week == 1 || 
      b == min(df$time_index, na.rm = TRUE)
    
    if (is_year_start) {
      # Returns Week over Year in brackets (e.g., "1\n(2025)")
      return(paste0(curr_week, "\n(", curr_year, ")"))
    } else {
      return(as.character(curr_week))
    }
  })
}

#' @title Internal Helper to Calculate Smart X-Axis Breaks
#'
#' @description
#' Ensures that the X-axis always has a tick mark at the exact start of a new 
#' year, while maintaining a standard 5-week interval for the rest of the plot.
#'
#' @param df Data frame containing the 'year' and 'time_index' columns.
#' @param x_var Character string. The column name for the X-axis.
#'
#' @return A sorted numeric vector of unique breaks.
get_smart_breaks <- function(df, x_var = "time_index") {
  # Force breaks at the first available time_index of every year
  year_starts <- df %>% 
    dplyr::group_by(year) %>% 
    dplyr::filter(time_index == min(time_index)) %>% 
    dplyr::pull(time_index)
  
  # Create a standard sequence of 5-week steps
  std_steps <- seq(min(df[[x_var]], na.rm = TRUE), 
                   max(df[[x_var]], na.rm = TRUE), by = 5)
  
  # Combine and sort to ensure no overlap
  return(sort(unique(c(year_starts, std_steps))))
}

#' @title Plot Observed vs Expected Counts for a Single Unit
#'
#' @description
#' Creates a combined plot for a single analysis unit showing observed weekly 
#' counts as bars, the expected baseline as a line, and alarm points. 
#' Uses smart X-axis breaks and vertical year labels.
#'
#' @param df_unit Data frame containing data for a single analysis unit.
#' @param unit_label Character (optional). The analysis_unit_id (cleans pipes automatically).
#' @param x_var Character string. Name of the column for the x-axis. Defaults to "time_index".
#' @param count_var Character string. Column name for observed counts. Defaults to "n_cases".
#' @param mu_var Character string. Column name for expected baseline counts. Defaults to "mu_hat".
#' @param alarm_var Character string. Column name for the logical alarm flag. Defaults to "alarm".
#'
#' @return A \code{ggplot2} object with a \code{theme_bw} and base size of 18.
#' @export
plot_cusum_series_unit <- function(df_unit,
                                   unit_label = NULL,
                                   x_var      = "time_index",
                                   count_var  = "n_cases",
                                   mu_var     = "mu_hat",
                                   alarm_var  = "alarm") {
  
  # Clean unit label if it contains pipes (e.g. Argentina|Catamarca|Ambato)
  clean_title <- if (!is.null(unit_label)) sub(".*\\|", "", unit_label) else "Observed vs Expected"
  
  x_sym     <- rlang::sym(x_var)
  count_sym <- rlang::sym(count_var)
  mu_sym    <- rlang::sym(mu_var)
  
  # Calculate dynamic breaks
  my_breaks <- get_smart_breaks(df_unit, x_var)
  
  ggplot(df_unit, aes(x = !!x_sym)) +
    geom_col(aes(y = !!count_sym), fill = "steelblue", alpha = 0.4) +
    geom_line(aes(y = !!mu_sym), color = "black", linewidth = 1) +
    geom_point(
      data = df_unit[df_unit[[alarm_var]] == TRUE, ],
      aes(y = !!count_sym), size = 4, color = "red"
    ) +
    scale_x_continuous(breaks = my_breaks, labels = function(b) smart_labs(b, df_unit)) +
    labs(x = "Timeline\n(Week / Year)", y = "Weekly counts", title = paste("Unit:", clean_title)) +
    theme_bw(base_size = 18) + 
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())
}

#' @title Plot CUSUM Process for a Single Unit
#'
#' @description
#' Visualizes the CUSUM statistic over time. Includes a red dashed 
#' threshold line and smart X-axis labels with vertical year stacking.
#'
#' @param df_unit Data frame containing data for a single analysis unit.
#' @param unit_label Character (optional). The analysis_unit_id (cleans pipes automatically).
#' @param x_var Character string. Name of the time column. Defaults to "time_index".
#' @param cusum_var Character string. Column name for the CUSUM statistic. Defaults to "cusum".
#' @param h Numeric. The value of the decision threshold. Defaults to 2.26.
#'
#' @return A \code{ggplot2} object with a \code{theme_minimal} and base size of 18.
#' @export
plot_cusum_process_unit <- function(df_unit,
                                    unit_label = NULL,
                                    x_var      = "time_index",
                                    cusum_var  = "cusum",
                                    h          = 2.26) {
  
  clean_title <- if (!is.null(unit_label)) sub(".*\\|", "", unit_label) else "CUSUM Process"
  x_sym     <- rlang::sym(x_var)
  cusum_sym <- rlang::sym(cusum_var)
  
  my_breaks <- get_smart_breaks(df_unit, x_var)
  
  ggplot(df_unit, aes(x = !!x_sym, y = !!cusum_sym)) +
    geom_line(linewidth = 1, color = "darkblue") +
    geom_hline(yintercept = h, linetype = "dashed", color = "red", linewidth = 1) +
    scale_x_continuous(breaks = my_breaks, labels = function(b) smart_labs(b, df_unit)) +
    labs(x = "Timeline\n(Week / Year)", y = "CUSUM Score (S_n)", title = paste("CUSUM - Unit:", clean_title)) +
    theme_minimal(base_size = 18) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank())
}

#' @title CUSUM Alarms Overview (Heatmap)
#'
#' @description
#' Generates a heatmap summarizing alarm status across multiple units. 
#' Automatically cleans analysis_unit_id labels and forces year changes 
#' on the X-axis.
#'
#' @param df Data frame containing the full dataset for multiple analysis units.
#' @param x_var Character string. Name of the time column. Defaults to "time_index".
#' @param unit_var Character string. Name of the grouping column. Defaults to "analysis_unit_id".
#' @param alarm_var Character string. Column name for the logical alarm flag. Defaults to "alarm".
#'
#' @return A \code{ggplot2} object with vertical year labeling and base size of 18.
#' @export
plot_cusum_alarms_overview <- function(df,
                                       x_var     = "time_index",
                                       unit_var  = "analysis_unit_id",
                                       alarm_var = "alarm") {
  
  # Clean unit labels to show only the last segment (e.g. "Ambato")
  df <- df %>% dplyr::mutate(clean_label = sub(".*\\|", "", !!rlang::sym(unit_var)))
  
  x_sym     <- rlang::sym(x_var)
  unit_sym  <- rlang::sym("clean_label")
  alarm_sym <- rlang::sym(alarm_var)
  
  my_breaks <- get_smart_breaks(df, x_var)
  
  ggplot(df, aes(x = !!x_sym, y = !!unit_sym, fill = !!alarm_sym)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_manual(values = c("FALSE" = "grey90", "TRUE" = "red")) +
    scale_x_continuous(breaks = my_breaks, labels = function(b) smart_labs(b, df)) +
    labs(x = "Timeline\n(Week / Year)", y = "Analysis unit", fill = "Alarm", title = "Outbreak Heatmap") +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x = element_text(angle = 0),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid = element_blank()
    )
}