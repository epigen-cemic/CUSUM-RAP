## -----------------------------------------------------------
## main.R
## WS5.WP1.2 - Product 1: CUSUM RAP
##
## Pipeline:
##   1) Read API-Pop weekly aggregated rumors (by location).
##   2) Prepare weekly time index and epi_date at selected geo level.
##   3) Define baseline and detection windows.
##   4) Run unit-level CUSUM for all analysis units.
##   5) Generate plots and save outputs.
## -----------------------------------------------------------



## -----------------------------------------------------------
## 1) User-configurable parameters
## -----------------------------------------------------------

# Geographic analysis level
# Options: "country", "province", "department", "censal_censal_fraction"
location_level <- "province"

# CUSUM threshold h (to be calibrated)
cusum_h <- 3

# Baseline length (in weeks):
#   - detection period = last baseline_length_weeks
#   - baseline period  = all weeks before that
baseline_length_weeks <- 52

# Input path: API-Pop aggregated CSV
input_path <- "input/rumor_counts_by_week_unit.csv"

# Output paths (will be created in 'output' folder)
output_csv_path     <- paste0("output/cusum_results_", location_level, ".csv")
output_series_png   <- paste0("output/example_", location_level, "_series.png")
output_cusum_png    <- paste0("output/example_", location_level, "_cusum.png")
output_overview_png <- paste0("output/cusum_alarms_overview_", location_level, ".png")


## -----------------------------------------------------------
## 2) Read input data
## -----------------------------------------------------------

raw_data <- read_api_pop_output(input_path)


## -----------------------------------------------------------
## 3) Prepare weekly data at the selected geographic level
## -----------------------------------------------------------

weekly_data <- prepare_weekly_data_geo(
  raw_data,
  location_level = location_level,
  col_country    = "country",
  col_province   = "province",
  col_department = "department",
  col_censal_fraction   = "censal_fraction",
  col_year       = NULL,      # no separate year column
  col_week       = NULL,      # no separate week column
  col_yearweek   = "week",    # your combined "year-week" column
  col_cases      = "n_cases"
)



## -----------------------------------------------------------
## 4) Define baseline and detection periods (per analysis unit)
## -----------------------------------------------------------

baseline_filter <- function(df_unit) {
  max_t <- max(df_unit$time_index, na.rm = TRUE)
  df_unit$time_index <= (max_t - baseline_length_weeks)
}

detect_filter <- function(df_unit) {
  max_t <- max(df_unit$time_index, na.rm = TRUE)
  df_unit$time_index > (max_t - baseline_length_weeks)
}


## -----------------------------------------------------------
## 5) Run CUSUM for all analysis units
## -----------------------------------------------------------

cusum_results <- run_cusum_all_units(
  df              = weekly_data,
  unit_var        = "analysis_unit_id",
  baseline_filter = baseline_filter,
  detect_filter   = detect_filter,
  k      = 1.04,
  h      = cusum_h,
  trans  = "standard",
  reset  = FALSE
)


## -----------------------------------------------------------
## 6) Save analytical output
## -----------------------------------------------------------

save_cusum_results(cusum_results, output_csv_path)


## -----------------------------------------------------------
## 7) Example plots for one analysis unit
## -----------------------------------------------------------

if (nrow(cusum_results) > 0) {
  
  example_unit <- unique(cusum_results$analysis_unit_id)[1]
  
  df_example <- cusum_results %>%
    dplyr::filter(analysis_unit_id == example_unit)
  
  p_series <- plot_cusum_series_unit(df_example, unit_label = example_unit)
  p_cusum  <- plot_cusum_process_unit(df_example, unit_label = example_unit,
                                      h = cusum_h)
  
  ggplot2::ggsave(output_series_png, p_series, width = 8, height = 4)
  ggplot2::ggsave(output_cusum_png,  p_cusum,  width = 8, height = 4)
  
  ## Overview across all units
  p_overview <- plot_cusum_alarms_overview(cusum_results,
                                           unit_var = "analysis_unit_id")
  ggplot2::ggsave(output_overview_png, p_overview, width = 10, height = 6)
}
