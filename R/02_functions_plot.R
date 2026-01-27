## -----------------------------------------------------------
## 02_functions_plot.R
## Plotting functions for CUSUM in WS5.WP1.2
## Rumor counts aggregated by spatial analysis unit and week
## -----------------------------------------------------------

library(ggplot2)
library(rlang)

## plot_cusum_series_unit:
##  - Plot observed vs expected counts for ONE analysis unit
##    (which may be a country, province, department, or fraction).
##
## Arguments:
##  - df_unit: data for a single analysis unit (already filtered)
##  - unit_label: optional label to show in the title
##  - x_var: variable to use on the x-axis (e.g. "epi_date")
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


## plot_cusum_process_unit:
##  - Plot the CUSUM process S_t over time for ONE analysis unit.
##  - Draws a horizontal line at the decision threshold h.
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


## plot_cusum_alarms_overview:
##  - Overview of alarms across all analysis units and weeks.
##  - Creates a tile/heatmap with units on one axis and time on the other,
##    highlighting weeks with alarms.
plot_cusum_alarms_overview <- function(df,
                                       x_var     = "epi_date",
                                       unit_var  = "analysis_unit_id",
                                       alarm_var = "alarm") {
  
  x_sym    <- rlang::sym(x_var)
  unit_sym <- rlang::sym(unit_var)
  alarm_sym<- rlang::sym(alarm_var)
  
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
