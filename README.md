# WP1.2 CUSUM RAP

This repository contains the scripts for WS5.WP1.2 Product 1:
a Reproducible Analytical Pipeline (RAP) implementing a CUSUM-based
early detection algorithm on weekly case counts per spatial unit.

The RAP is designed to run **after**:
1. Individual-level case data are collected with **Rapid Case Report**.
2. Data are filtered and aggregated by week and spatial unit using **API-Pop**.
3. The aggregated output (weekly counts) is saved as a CSV and used as input
   for this CUSUM RAP.

## Repository structure

- `R/01_functions_cusum.R`  
  Core analytical functions:
  - `cusum_core()`: implements the one-sided CUSUM for counts.
  - `run_cusum_for_unit()`: runs CUSUM for a single analysis unit.
  - `run_cusum_all_units()`: runs CUSUM for all units in the dataset.

- `R/02_functions_plot.R`  
  Plotting helpers:
  - `plot_cusum_series_unit()`: observed vs expected counts with alarms.
  - `plot_cusum_process_unit()`: CUSUM process over time.
  - `plot_cusum_alarms_overview()`: tile plot of alarms across all units.

- `R/03_functions_io.R`  
  Input/output helpers:
  - `read_api_pop_output()`: reads API-Pop aggregated CSV.
  - `prepare_weekly_data_geo()`: aggregates weekly counts at selected
    geographic level and builds `analysis_unit_id`, `epi_date`, `time_index`.
  - `save_cusum_results()`: writes CUSUM-enriched output to CSV.

- `main.R`  
  Main script orchestrating the RAP:
  - reads input CSV,
  - prepares weekly data for the chosen geographic level,
  - defines baseline and detection windows,
  - runs CUSUM for all units,
  - saves results and example plots.

- `input/`  
  Folder for API-Pop aggregated CSV files.  

  The input CSV produced by API-Pop must include the spatial location columns
  (e.g. `country`, `province`, `department`, `censal_censal_fraction`) and the weekly
  counts column (e.g. `n_cases`). For the time dimension, the RAP accepts
  **two alternative formats**:

  - Either **separate `year` and `week` columns** (e.g. `year = 2024`,
    `week = 5`), or  
  - A **single combined `week` column** with values in `"year-week"` format
    (e.g. `"2024-05"` or `"2024-5"`).

  Internally, the helper function `prepare_weekly_data_geo()` parses this
  information and creates the `year`, `week`, `epi_date`, and `time_index`
  variables used by the CUSUM analysis. The mapping between input column names
  and internal variables can be configured via the arguments `col_year`,
  `col_week`, and `col_yearweek` in `prepare_weekly_data_geo()`.

- `output/`  
  Folder for CUSUM results and figures (CSV and PNG).

## R dependencies

This RAP uses the following R packages:

- `dplyr`
- `ggplot2`
- `readr`
- `lubridate`
- `ISOweek`
- `tidyr`
- `rlang`

You can install them with:

```r
install.packages(c(
  "dplyr", "ggplot2", "readr",
  "lubridate", "ISOweek", "tidyr", "rlang"
))
