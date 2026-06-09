## -----------------------------------------------------------
## functions_plot.R
## Plotting functions for CUSUM with Smart X-Axis and Clean IDs
## -----------------------------------------------------------

# Brand colours used in CUSUM plots. Keep these aligned with www/config.css.
cusum_plot_colours <- list(
  neutral   = "#E5E2DC",
  no_alarm  = "#B8C7A8",
  alarm     = "#B4443F",
  baseline  = "#000000",
  accent    = "#F69342",
  blue      = "#48563F",
  dark      = "#1E2A15"
)

clean_unit_label <- function(x, max_width = NULL) {
  out <- sub(".*\\|", "", x)
  if (!is.null(max_width) && requireNamespace("stringr", quietly = TRUE)) {
    out <- stringr::str_trunc(out, max_width)
  }
  out
}

make_heatmap_location_labels <- function(df, unit_var = "analysis_unit_id", max_width = NULL) {
  if (!unit_var %in% names(df)) {
    return(rep(NA_character_, nrow(df)))
  }

  geo_cols <- intersect(c("country", "level1", "level2", "level3", "level4"), names(df))
  if (length(geo_cols) == 0) {
    return(clean_unit_label(df[[unit_var]], max_width = max_width))
  }

  unit_geo <- df %>%
    dplyr::select(dplyr::all_of(c(unit_var, geo_cols))) %>%
    dplyr::distinct()

  deepest_col <- geo_cols[length(geo_cols)]
  parent_col <- if (length(geo_cols) >= 2) geo_cols[length(geo_cols) - 1] else NULL

  unit_geo <- unit_geo %>%
    dplyr::mutate(
      .base_label = as.character(.data[[deepest_col]]),
      .base_label = dplyr::if_else(is.na(.base_label) | .base_label == "", clean_unit_label(.data[[unit_var]]), .base_label)
    )

  duplicated_base <- unit_geo$.base_label[duplicated(unit_geo$.base_label) | duplicated(unit_geo$.base_label, fromLast = TRUE)]

  if (!is.null(parent_col) && length(duplicated_base) > 0) {
    unit_geo <- unit_geo %>%
      dplyr::mutate(
        .parent_label = as.character(.data[[parent_col]]),
        .clean_label = dplyr::if_else(
          .base_label %in% duplicated_base & !is.na(.parent_label) & .parent_label != "",
          paste(.parent_label, .base_label, sep = " / "),
          .base_label
        )
      )
  } else {
    unit_geo$.clean_label <- unit_geo$.base_label
  }

  if (!is.null(max_width) && requireNamespace("stringr", quietly = TRUE)) {
    unit_geo$.clean_label <- stringr::str_trunc(unit_geo$.clean_label, max_width)
  }

  lookup <- stats::setNames(unit_geo$.clean_label, unit_geo[[unit_var]])
  unname(lookup[as.character(df[[unit_var]])])
}


add_prevalence_plot_columns <- function(df_unit,
                                        count_var = "n_cases",
                                        mu_var = "mu_hat",
                                        population_var = "population") {
  has_population <- population_var %in% names(df_unit) &&
    any(!is.na(df_unit[[population_var]]) & df_unit[[population_var]] > 0)

  if (!has_population) {
    df_unit$.plot_observed <- df_unit[[count_var]]
    df_unit$.plot_expected <- df_unit[[mu_var]]
    attr(df_unit, "plot_y_label") <- "Case count"
    attr(df_unit, "plot_subtitle") <- "Observed weekly counts compared with the expected baseline."
    attr(df_unit, "plot_caption") <- "Black line = expected baseline frequency.\nRed points = weeks with CUSUM alarms. Population data were not available, so this plot shows case counts instead of prevalence."
    return(df_unit)
  }

  population <- df_unit[[population_var]]
  population[is.na(population) | population <= 0] <- NA_real_

  df_unit$.plot_observed <- (df_unit[[count_var]] / population) * 100000
  df_unit$.plot_expected <- (df_unit[[mu_var]] / population) * 100000
  attr(df_unit, "plot_y_label") <- "Prevalence"
  attr(df_unit, "plot_subtitle") <- "Observed weekly values compared with the expected baseline per 100,000 people."
  attr(df_unit, "plot_caption") <- "Black line = expected baseline prevalence.\nRed points = weeks with CUSUM alarms."
  df_unit
}

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
#' @keywords internal
#' @noRd
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
#' year and at the very last available data point, while maintaining a standard 
#' 5-week interval for the rest of the timeline.
#'
#' @param df Data frame containing the 'year' and the time index columns.
#' @param x_var Character string. The column name for the continuous X-axis. 
#'        Defaults to "time_index".
#'
#' @return A sorted numeric vector of unique breaks to be used by \code{scale_x_continuous}.
#' 
#' @keywords internal
#' @noRd
get_smart_breaks <- function(df, x_var = "time_index") {
  year_starts <- df %>% 
    dplyr::group_by(year) %>% 
    dplyr::filter(time_index == min(time_index)) %>% 
    dplyr::pull(time_index)
  
  max_val <- max(df[[x_var]], na.rm = TRUE)

  std_steps <- seq(min(df[[x_var]], na.rm = TRUE), 
                   max_val, by = 5)

  breaks <- sort(unique(c(year_starts, std_steps, max_val)))

  # Avoid crowding labels at the end of the axis, e.g. showing both 51 and 52.
  if (length(breaks) >= 2) {
    before_last <- breaks[length(breaks) - 1]
    if ((max_val - before_last) < 3 && before_last %in% std_steps && before_last != min(breaks)) {
      breaks <- setdiff(breaks, before_last)
    }
  }

  return(sort(unique(breaks)))
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
                                   alarm_var  = "alarm",
                                   base_size  = 18) {
  
  # Clean unit label if it contains pipes (e.g. Argentina|Catamarca|Ambato)
  clean_title <- if (!is.null(unit_label)) clean_unit_label(unit_label) else "Observed vs Expected"
  df_unit <- add_prevalence_plot_columns(df_unit, count_var = count_var, mu_var = mu_var)
  
  x_sym <- rlang::sym(x_var)
  
  # Calculate dynamic breaks
  my_breaks <- get_smart_breaks(df_unit, x_var)
  
  ggplot(df_unit, aes(x = !!x_sym)) +
    geom_col(aes(y = .plot_observed), fill = cusum_plot_colours$blue, alpha = 0.45) +
    geom_line(aes(y = .plot_expected), color = "black", linewidth = 1) +
    geom_point(
      data = df_unit[df_unit[[alarm_var]] == TRUE, ],
      aes(y = .plot_observed), size = 4, color = cusum_plot_colours$alarm
    ) +
    scale_x_continuous(breaks = my_breaks, labels = function(b) smart_labs(b, df_unit)) +
    labs(
      x = "Timeline\n(Week / Year)",
      y = attr(df_unit, "plot_y_label"),
      title = paste("Unit:", clean_title),
      subtitle = attr(df_unit, "plot_subtitle"),
      caption = attr(df_unit, "plot_caption")
    ) +
    theme_bw(base_size = base_size) + 
    theme(
          plot.title = element_text(face = "bold", margin = margin(b = 6)),
          plot.subtitle = element_text(size = base_size * 0.82, color = "gray20", margin = margin(b = 10)),
          plot.caption = element_text(size = base_size * 0.70, hjust = 0, lineheight = 1.10, margin = margin(t = 8)),
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.title.y = element_text(margin = margin(r = 12)),
          plot.margin = margin(12, 16, 18, 18),
          panel.grid.minor = element_blank())
}

#' @title Plot CUSUM Process for a Single Unit
#'
#' @description
#' Visualizes the CUSUM statistic over time. Includes a red dashed 
#' threshold line, red dots for ALL alarms, and smart X-axis labels.
#'
#' @param df_unit Data frame containing data for a single analysis unit.
#' @param unit_label Character (optional). The analysis_unit_id.
#' @param x_var Character string. Name of the time column. Defaults to "time_index".
#' @param cusum_var Character string. Column name for the CUSUM statistic. Defaults to "cusum".
#' @param alarm_var Character string. Column name for the logical alarm flag. Defaults to "alarm".
#' @param h Numeric. The value of the decision threshold. Defaults to 2.26.
#' @param k Numeric. The reference value. Defaults to 1.04.
#' @param arl0 Numeric. The ARL0 value. Defaults to 370.
#'
#' @return A \code{ggplot2} object with a \code{theme_minimal} and base size of 18.
#' @export
plot_cusum_process_unit <- function(df_unit,
                                    unit_label = NULL,
                                    x_var      = "time_index",
                                    cusum_var  = "cusum",
                                    alarm_var  = "alarm",
                                    h          = 2.26,
                                    k          = 1.04,
                                    arl0       = 370,
                                    rr         = NA_real_,
                                    rate_per_100k = NA_real_,
                                    base_size  = 18) {
  
  # Clean the title (e.g., remove "Argentina|")
  clean_title <- if (!is.null(unit_label)) clean_unit_label(unit_label) else "CUSUM Process"
  
  x_sym     <- rlang::sym(x_var)
  cusum_sym <- rlang::sym(cusum_var)
  
  # Calculate smart x-axis breaks
  my_breaks <- get_smart_breaks(df_unit, x_var)
  
  # Filter data to get ONLY the rows where an alarm occurred
  alarms_only <- df_unit[df_unit[[alarm_var]] == TRUE, ]
  
  # Format the subtitle to display parameters
  rr_label <- if (is.na(rr)) "NA" else sprintf("%.3f", rr)
  rate_label <- if (is.na(rate_per_100k)) "NA" else sprintf("%.3f", rate_per_100k)
  param_subtitle <- sprintf(
    "ARL0= %.2f   RR= %s   h= %.3f   k= %.3f   rate per 100,000 people= %s",
    arl0, rr_label, h, k, rate_label
  )

  y_breaks <- sort(unique(c(pretty(df_unit[[cusum_var]]), h)))
  y_labels <- function(x) {
    ifelse(abs(x - h) < .Machine$double.eps^0.5, paste0("h = ", format(round(h, 3), nsmall = 3)), as.character(x))
  }
  
  ggplot(df_unit, aes(x = !!x_sym, y = !!cusum_sym)) +
    # 1. The main blue line (CUSUM Score)
    geom_line(linewidth = 1, color = cusum_plot_colours$blue) +
    
    # 2. The dashed red threshold line
    geom_hline(yintercept = h, linetype = "dashed", color = cusum_plot_colours$alarm, linewidth = 1) +
    
    # 3. Red dots for ALL alarms (Real-World Standard)
    geom_point(data = alarms_only, aes(y = !!cusum_sym), size = 4, color = cusum_plot_colours$alarm) +
    
    scale_x_continuous(breaks = my_breaks, labels = function(b) smart_labs(b, df_unit)) +
    scale_y_continuous(breaks = y_breaks, labels = y_labels) +
    labs(x = "Timeline\n(Week / Year)", 
         y = "CUSUM Score (S_n)", 
         title = "Standardized Residual CUSUM (reset at alarms)",
         subtitle = param_subtitle,
         caption = "Red dashed line = h decision threshold. Red points = weeks where the CUSUM score reached or crossed h.\nThe CUSUM score is based on standardized weekly deviations; interpret location burden with prevalence per 100,000 people when population data are available.") +
    theme_minimal(base_size = base_size) +
    theme(
          plot.title = element_text(face = "bold", margin = margin(b = 6)),
          plot.subtitle = element_text(size = base_size * 0.78, color = "gray20", margin = margin(b = 10)),
          plot.caption = element_text(size = base_size * 0.70, hjust = 0, lineheight = 1.10, margin = margin(t = 8)),
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.title.y = element_text(margin = margin(r = 12)),
          plot.margin = margin(12, 16, 18, 28),
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
                                       alarm_var = "alarm",
                                       compact   = FALSE,
                                       base_size = 18) {

  # Clean unit labels using the selected hierarchy level. If two units share
  # the same displayed name, include the parent level to disambiguate them.
  label_width <- if (isTRUE(compact)) 18 else 35
  df$clean_label <- make_heatmap_location_labels(df, unit_var = unit_var, max_width = label_width)

  x_sym     <- rlang::sym(x_var)
  unit_sym  <- rlang::sym("clean_label")
  alarm_sym <- rlang::sym(alarm_var)

  my_breaks <- get_smart_breaks(df, x_var)

  p <- ggplot(df, aes(x = !!x_sym, y = !!unit_sym, fill = !!alarm_sym)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_manual(
      values = c(
        "FALSE" = "#B8C7A8",
        "TRUE" = cusum_plot_colours$alarm
      ),
      labels = c(
        "FALSE" = "No alarm detected",
        "TRUE" = "Alarm detected"
      ),
      name = "CUSUM status"
    ) +
    guides(
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 1,
        byrow = TRUE,
        keywidth = grid::unit(0.8, "cm"),
        keyheight = grid::unit(0.45, "cm")
      )
    ) +
    scale_x_continuous(breaks = my_breaks, labels = function(b) smart_labs(b, df)) +
    labs(
      x = "Timeline\n(Week / Year)",
      y = if (isTRUE(compact)) NULL else "Location",
      title = "Outbreak Heatmap",
      subtitle = "Red cells show weeks where the CUSUM threshold was exceeded. Green/grey cells show weeks with no alarm detected."
    ) +
    theme_minimal(base_size = if (isTRUE(compact)) max(13, base_size - 5) else base_size) +
    theme(
      axis.text.x = element_text(angle = 0),
      axis.text.y = if (isTRUE(compact)) element_blank() else element_text(),
      axis.ticks.y = if (isTRUE(compact)) element_blank() else element_line(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(
        size = if (isTRUE(compact)) max(11, base_size * 0.70) else base_size * 0.80,
        color = "gray30"
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(size = max(11, base_size * 0.75)),
      legend.text = element_text(size = max(10, base_size * 0.70)),
      legend.key.size = grid::unit(max(0.45, base_size / 28), "cm"),
      panel.grid = element_blank()
    )
  
  p
}
