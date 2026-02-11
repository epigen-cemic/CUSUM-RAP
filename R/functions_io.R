## -----------------------------------------------------------
## 03_functions_io.R
## Input/Output helpers for WS5.WP1.2 CUSUM RAP
## Reading aggregated API-Pop output and preparing data 
## using dynamic geographic configuration.
## -----------------------------------------------------------

#' @title Read API-Pop Output
#'
#' @description
#' Reads the weekly aggregated rumor data produced by API-Pop.
#' Expects a CSV file containing geographic columns and weekly case counts.
#'
#' @param path Character string. The file path to the CSV file.
#'
#' @return A tibble (data frame) containing the raw data.
#'
#' @importFrom readr read_csv
#' @export
read_api_pop_output <- function(path) {
  read_csv(path, show_col_types = FALSE)
}


#' @title Prepare Weekly Data (Standardized Columns)
#'
#' @description
#' Aggregates weekly data based on a configuration hierarchy, but expects 
#' standardized CSV column names (country, level1, level2...) instead of 
#' specific names (province, department, etc.).
#'
#' @details
#' This function acts as a translation layer between the user-facing configuration
#' and the internal data structure.
#' \enumerate{
#'   \item It accepts a user-selected level from the config (e.g., "Department").
#'   \item It maps this level to a standardized column depth (e.g., Department = Index 3 = "level2").
#'   \item It aggregates the data using these standardized columns.
#' }
#' 
#' The input CSV \strong{must} use the following standardized headers:
#' \itemize{
#'   \item \code{country}
#'   \item \code{level1} (e.g., Province/Region)
#'   \item \code{level2} (e.g., Department/District)
#'   \item \code{level3} (Optional)
#'   \item \code{level4} (Optional)
#' }
#'
#' @param df Data frame. The raw data containing standardized columns (\code{level1}, \code{level2}, etc.).
#' @param hierarchy_levels Character vector. The display names from the config file (e.g. \code{c("Country", "Province", "Department")}).
#' @param selected_level Character string. The specific display name selected by the user (e.g. "Department").
#'                       Must be present in \code{hierarchy_levels}.
#' @param col_year Character string. Name of the year column in \code{df}. Defaults to "year".
#' @param col_week Character string. Name of the week column in \code{df}. Defaults to "week".
#' @param col_yearweek Character string. Name of the combined year-week column if separate 
#'                     columns do not exist. Defaults to "week".
#' @param col_cases Character string. Name of the case count column. Defaults to "n_cases".
#'
#' @return A tibble with the following columns:
#' \item{analysis_unit_id}{Unique ID created by pipe-separating the generic levels (e.g. "Argentina|Buenos Aires|La Plata").}
#' \item{n_cases}{Aggregated counts.}
#' \item{epi_date}{Approximate date for plotting.}
#' \item{time_index}{Sequential integer index.}
#' \item{...}{The generic geographic columns used for grouping (country, level1, etc.).}
#'
#' @import dplyr
#' @import tidyr
#' @importFrom ISOweek ISOweek2date
#' @export
prepare_weekly_data_geo <- function(df,
                                    hierarchy_levels,
                                    selected_level,
                                    col_year       = "year",
                                    col_week       = "week",
                                    col_yearweek   = "week",
                                    col_cases      = "n_cases") {
  
  
  # ------------------------------------------------------------------
  # 0. NORMALIZE COLUMN NAMES
  # ------------------------------------------------------------------
  # Convert all column names to lowercase to handle "Level1", "LEVEL1", "Year", etc.
  names(df) <- tolower(names(df))
  
  # ------------------------------------------------------------------
  # 1. Define the Standardized Map
  # ------------------------------------------------------------------
  # Indices correspond to your Config levels. 
  # Index 1 is always Country. 
  # Index 2 is Level 1, Index 3 is Level 2, etc.
  generic_map <- c("country", "level1", "level2", "level3", "level4")
  
  # Validation: Check if selected level exists in the hierarchy
  if (!selected_level %in% hierarchy_levels) {
    stop(paste("Error: Selected level '", selected_level, 
               "' is not found in the provided hierarchy configuration."))
  }
  
  # ------------------------------------------------------------------
  # 2. Determine Required Generic Columns
  # ------------------------------------------------------------------
  # Find the depth of the selected level (e.g., "Department" might be index 3)
  level_depth <- which(hierarchy_levels == selected_level)
  
  # Ensure we don't exceed the map (max 4 levels + country)
  if (level_depth > length(generic_map)) {
    stop(paste("Error: The requested hierarchy depth (", level_depth, 
               ") exceeds the supported maximum (Country + 4 levels)."))
  }
  
  # Select the generic columns required for this depth
  # e.g., if depth is 3, we need: "country", "level1", "level2"
  required_cols <- generic_map[1:level_depth]
  
  # ------------------------------------------------------------------
  # 3. Check CSV for these Generic Columns
  # ------------------------------------------------------------------
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("Error: The uploaded CSV is missing the standardized columns required for this level:", 
               paste(missing_cols, collapse = ", "), 
               ". Please ensure your CSV uses headers 'country', 'level1', 'level2', etc."))
  }
  
  # ------------------------------------------------------------------
  # 4. Handle Time Variables
  # ------------------------------------------------------------------
  has_year_week <- (col_year %in% names(df)) && (col_week %in% names(df))
  
  if (has_year_week) {
    # Case A: Explicit Year and Week columns exist
    df <- df %>%
      dplyr::rename(
        year = !!col_year,
        week = !!col_week
      )
  } else if (col_yearweek %in% names(df)) {
    # Case B: Combined string (e.g. "2024-W01")
    df <- df %>%
      dplyr::rename(yearweek = !!col_yearweek) %>%
      tidyr::separate(
        yearweek,
        into    = c("year", "week"),
        sep     = "[-_/W]",  
        remove  = TRUE,
        convert = TRUE
      )
  } else {
    stop("Error: Could not find time columns. Need either (year & week) or a combined year-week column.")
  }
  
  # ------------------------------------------------------------------
  # 5. Aggregate Data using GENERIC columns
  # ------------------------------------------------------------------
  # We group by the dynamic required_cols + year + week
  df_agg <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(required_cols, "year", "week")))) %>%
    dplyr::summarise(
      n_cases = sum(.data[[col_cases]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # ------------------------------------------------------------------
  # 6. Create Analysis Unit ID
  # ------------------------------------------------------------------
  # Concatenate the geo levels with "|" to create a unique ID
  df_agg <- df_agg %>%
    tidyr::unite(
      "analysis_unit_id",
      dplyr::all_of(required_cols),
      sep = "|",
      remove = FALSE
    )
  
  # ------------------------------------------------------------------
  # 7. Add Date and Time Index
  # ------------------------------------------------------------------
  df_agg <- df_agg %>%
    dplyr::mutate(
      iso_string = sprintf("%04d-W%02d-1", year, week),
      epi_date   = ISOweek::ISOweek2date(iso_string)
    ) %>%
    dplyr::select(-iso_string) %>%
    # Sort and Index
    dplyr::arrange(analysis_unit_id, year, week) %>%
    dplyr::group_by(analysis_unit_id) %>%
    dplyr::mutate(time_index = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  return(df_agg)
}


#' @title Save CUSUM Results
#'
#' @description
#' Writes the CUSUM-enriched dataset to a CSV file.
#'
#' @param df Data frame. The dataset containing the CUSUM results.
#' @param path Character string. The file path where the CSV should be saved.
#'
#' @return The input \code{df} (invisibly).
#'
#' @importFrom readr write_csv
#' @export
save_cusum_results <- function(df, path) {
  write_csv(df, path)
}