## -----------------------------------------------------------
## 03_functions_io.R
## Input/Output helpers for WS5.WP1.2 CUSUM RAP
## Reading aggregated API-Pop output (weekly counts by location)
## and preparing data at the selected geographic level
## -----------------------------------------------------------

library(readr)
library(dplyr)
library(lubridate)
library(ISOweek)
library(tidyr)

## read_api_pop_output:
##  - Reads the weekly aggregated rumor data produced by API-Pop.
##  - Assumes at least the following columns:
##      country, province, department, censal_censal_fraction,
##      year, week, n_cases
##  - If your file uses different names, adapt the prepare function.
read_api_pop_output <- function(path) {
  read_csv(path, show_col_types = FALSE)
}


## prepare_weekly_data_geo:
##  - Standardizes column names.
##  - Accepts either:
##      (a) separate year and week columns, or
##      (b) a single year-week column (e.g. "2024-05" or "2024-5").
##  - Aggregates weekly counts at a chosen geographic level:
##      "country", "province", "department", or "censal_fraction".
##  - Creates:
##      analysis_unit_id, epi_date, time_index.
##
## Arguments:
##  - df: raw data as read from API-Pop output.
##  - location_level: one of "country", "province", "department", "censal_fraction".
##  - col_country, col_province, col_department, col_censal_fraction:
##        names of the geographic columns in the input.
##  - col_year, col_week:
##        names of the year and week columns (if they exist separately).
##  - col_yearweek:
##        name of the combined "year-week" column (if used).
prepare_weekly_data_geo <- function(df,
                                    location_level = c("country",
                                                       "province",
                                                       "department",
                                                       "censal_fraction"),
                                    col_country    = "country",
                                    col_province   = "province",
                                    col_department = "department",
                                    col_censal_fraction   = "censal_fraction",
                                    col_year       = NULL,
                                    col_week       = NULL,
                                    col_yearweek   = "week",
                                    col_cases      = "n_cases") {
  
  location_level <- match.arg(location_level)
  
  ## 1) Rename geographic and case-count columns
  df <- df %>%
    dplyr::rename(
      country    = !!col_country,
      province   = !!col_province,
      department = !!col_department,
      censal_fraction   = !!col_censal_fraction,
      n_cases    = !!col_cases
    )
  
  ## 2) Handle time variables: either (year + week) or a single year-week column
  if (!is.null(col_year) && !is.null(col_week)) {
    # Case A: year and week provided separately
    df <- df %>%
      dplyr::rename(
        year = !!col_year,
        week = !!col_week
      )
    
  } else if (!is.null(col_yearweek)) {
    # Case B: single "year-week" column, e.g. "2024-05" or "2024-5"
    df <- df %>%
      dplyr::rename(yearweek = !!col_yearweek) %>%
      tidyr::separate(
        yearweek,
        into    = c("year", "week"),
        sep     = "[-_/W]",  # split on "-", "_", "/", or "W"
        remove  = TRUE,
        convert = TRUE       # convert to numeric if possible
      )
  } else {
    stop("Either (col_year & col_week) or col_yearweek must be provided.")
  }
  
  ## 3) Choose which geographic variables define the analysis unit
  geo_vars <- switch(
    location_level,
    country    = c("country"),
    province   = c("country", "province"),
    department = c("country", "province", "department"),
    censal_fraction   = c("country", "province", "department", "censal_fraction")
  )
  
  ## 4) Aggregate weekly counts at the chosen geographic level
  df_agg <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(geo_vars, "year", "week")))) %>%
    dplyr::summarise(
      n_cases = sum(n_cases, na.rm = TRUE),
      .groups = "drop"
    )
  
  ## 5) Create a compact identifier for the analysis unit
  df_agg <- df_agg %>%
    tidyr::unite(
      "analysis_unit_id",
      dplyr::all_of(geo_vars),
      sep = "|",
      remove = FALSE
    )
  
  ## 6) Create an approximate epi_date for plotting (Monday of ISO week)
  df_agg <- df_agg %>%
    dplyr::mutate(
      iso_string = sprintf("%04d-W%02d-1", year, week),
      epi_date   = ISOweek::ISOweek2date(iso_string)
    ) %>%
    dplyr::select(-iso_string)
  
  ## 7) Sort and create time_index within each analysis unit
  df_agg <- df_agg %>%
    dplyr::arrange(analysis_unit_id, year, week) %>%
    dplyr::group_by(analysis_unit_id) %>%
    dplyr::mutate(time_index = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  df_agg
}


## save_cusum_results:
##  - Writes the CUSUM-enriched dataset to CSV.
##  - This is the main analytical output of Product 1 (WP1.2).
save_cusum_results <- function(df, path) {
  write_csv(df, path)
}
