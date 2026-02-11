## -----------------------------------------------------------
## 02_functions_plot.R
## Plotting functions for CUSUM in WS5.WP1.2
## Rumor counts aggregated by spatial analysis unit and week
## -----------------------------------------------------------

#' @title Plot Observed vs Expected Counts for a Single Unit
#'
#' @description
#' Creates a combined plot for a single analysis unit showing:
#' \itemize{
#'   \item Observed weekly counts (grey bars).
#'   \item Expected baseline counts (line).
#'   \item Alarm points (dots) where the CUSUM threshold was exceeded.
#' }
#'
#' @param df_unit Data frame. Contains data for a single analysis unit.
#'                Must be pre-filtered before passing to this function.
#' @param unit_label Character (optional). A label to append to the plot title
#'                   (e.g., the name of the province or department). Defaults to \code{NULL}.
#' @param x_var Character string. The name of the column to use for the x-axis
#'              (typically a Date object like \code{"epi_date"}). Defaults to \code{"epi_date"}.
#' @param count_var Character string. The column name for the observed counts. Defaults to \code{"n_cases"}.
#' @param mu_var Character string. The column name for the expected counts (baseline). Defaults to \code{"mu_hat"}.
#' @param alarm_var Character string. The column name for the logical alarm flag. Defaults to \code{"alarm"}.
#'
#' @return A \code{ggplot2} object.
#'
#' @import ggplot2
#' @importFrom rlang sym
#' @export
plot_cusum_series_unit <- function(df_unit,
                                   unit_label = NULL,
                                   x_var      = "epi_date",
                                   count_var  = "n_cases",
                                   mu_var     = "mu_hat",
                                   alarm_var  = "alarm") {
  
  x_sym     <- rlang::sym(x_var)
  count_sym <- rlang::sym(count_var)
  mu_sym    <- rlang::sym(mu_var)
  
  title_text <- if (is.null(unit_label)) {
    "Observed vs expected counts (CUSUM)"
  } else {
    paste("Observed vs expected counts - unit:", unit_label)
  }
  
  ggplot(df_unit, aes(x = !!x_sym)) +
    geom_col(aes(y = !!count_sym), alpha = 0.4) +
    geom_line(aes(y = !!mu_sym)) +
    geom_point(
      data = df_unit[df_unit[[alarm_var]] == TRUE, ],
      aes(y = !!count_sym),
      size = 2
    ) +
    labs(
      x = x_var,
      y = "Weekly counts",
      title = title_text
    )
}


#' @title Plot CUSUM Process for a Single Unit
#'
#' @description
#' Visualizes the CUSUM statistic (\eqn{S_t}) over time for a single analysis unit.
#' Draws a dashed horizontal line at the specified decision threshold (\eqn{h}).
#'
#' @param df_unit Data frame. Contains data for a single analysis unit.
#' @param unit_label Character (optional). A label to append to the plot title. Defaults to \code{NULL}.
#' @param x_var Character string. The name of the time column. Defaults to \code{"epi_date"}.
#' @param cusum_var Character string. The column name for the CUSUM statistic. Defaults to \code{"cusum"}.
#' @param h Numeric. The value of the decision threshold to draw as a reference line. Defaults to 2.26.
#'
#' @return A \code{ggplot2} object.
#'
#' @import ggplot2
#' @importFrom rlang sym
#' @export
plot_cusum_process_unit <- function(df_unit,
                                    unit_label = NULL,
                                    x_var      = "epi_date",
                                    cusum_var  = "cusum",
                                    h          = 2.26) {
  
  x_sym     <- rlang::sym(x_var)
  cusum_sym <- rlang::sym(cusum_var)
  
  title_text <- if (is.null(unit_label)) {
    "CUSUM process over time"
  } else {
    paste("CUSUM process - unit:", unit_label)
  }
  
  ggplot(df_unit, aes(x = !!x_sym, y = !!cusum_sym)) +
    geom_line() +
    geom_hline(yintercept = h, linetype = "dashed") +
    labs(
      x = x_var,
      y = "CUSUM",
      title = title_text
    )
}


#' @title CUSUM Alarms Overview (Heatmap)
#'
#' @description
#' Creates a heatmap (tile plot) summarizing the alarm status across all analysis units and time points.
#' This provides a high-level view of where and when outbreaks are potentially occurring.
#'
#' @details
#' \itemize{
#'   \item \strong{White tiles}: No alarm.
#'   \item \strong{Red tiles}: Alarm triggered (Threshold exceeded).
#' }
#'
#' @param df Data frame. The full dataset containing multiple analysis units.
#' @param x_var Character string. The name of the time column. Defaults to \code{"epi_date"}.
#' @param unit_var Character string. The name of the grouping column (e.g., province name).
#'                 Defaults to \code{"analysis_unit_id"}.
#' @param alarm_var Character string. The column name for the logical alarm flag. Defaults to \code{"alarm"}.
#'
#' @return A \code{ggplot2} object.
#'
#' @import ggplot2
#' @importFrom rlang sym
#' @export
plot_cusum_alarms_overview <- function(df,
                                       x_var     = "epi_date",
                                       unit_var  = "analysis_unit_id",
                                       alarm_var = "alarm") {
  
  x_sym     <- rlang::sym(x_var)
  unit_sym  <- rlang::sym(unit_var)
  alarm_sym <- rlang::sym(alarm_var)
  
  ggplot(df, aes(x = !!x_sym, y = !!unit_sym, fill = !!alarm_sym)) +
    geom_tile() +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red")) +
    labs(
      x = x_var,
      y = "Analysis unit",
      fill = "Alarm",
      title = "CUSUM alarms by analysis unit and week"
    )
}