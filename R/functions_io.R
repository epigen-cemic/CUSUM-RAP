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
#' Automatically detects if the file uses commas (,) or semicolons (;) as separators.
#'
#' @param path Character string. The file path to the CSV file.
#'
#' @return A tibble (data frame) containing the raw data.
#'
#' @importFrom readr read_delim read_csv read_csv2
#' @export
read_api_pop_output <- function(path) {
  # Read the first line to guess the separator
  first_line <- readLines(path, n = 1)
  
  # Check if the header contains a semicolon
  if (grepl(";", first_line)) {
    # Use read_csv2 for semicolon-separated files (common in LATAM/Europe)
    return(readr::read_csv2(path, show_col_types = FALSE))
  } else {
    # Use read_csv for standard comma-separated files
    return(readr::read_csv(path, show_col_types = FALSE))
  }
}


#' @title Combine and Filter Multiple Uploaded Files
#'
#' @description
#' Reads multiple uploaded CSV files, binds their rows into a single dataset, 
#' standardizes the column names to lowercase, and filters the dataset based 
#' on user-selected target locations. This "lazy filtering" prevents memory 
#' overload before applying the gap-filling algorithms.
#'
#' @param file_paths Character vector. The temporary file paths from \code{input$file_upload$datapath}.
#' @param target_locations Character vector. The specific locations to keep (e.g., c("Comuna 1", "Comuna 2")).
#' @param geo_col Character string. The standardized column name to apply the filter to (e.g., "level2").
#' @param read_fn Function. The function to use for reading the files. Defaults to \code{read.csv}.
#'
#' @return A data frame containing only the combined records for the requested target locations.
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter
#' @export
combine_and_filter_data <- function(file_paths, 
                                    target_locations = NULL, 
                                    geo_col = NULL,
                                    read_fn = read.csv) {
  
  # 1. Read and bind all uploaded files
  raw_df <- purrr::map_dfr(file_paths, read_fn)
  
  # 2. Standardize column names
  names(raw_df) <- tolower(names(raw_df))
  
  # 3. Apply memory-saving filter (if locations and a target column are provided)
  if (!is.null(target_locations) && !is.null(geo_col) && length(target_locations) > 0) {
    # Ensure the column exists before filtering to avoid errors
    if (geo_col %in% names(raw_df)) {
      raw_df <- raw_df %>%
        dplyr::filter(.data[[geo_col]] %in% target_locations)
    } else {
      warning(paste("Filter column", geo_col, "not found in uploaded data."))
    }
  }
  
  return(raw_df)
}


#' @title Prepare Weekly Data for Geographic Analysis
#'
#' @description
#' Standardizes raw data into a weekly time series format for CUSUM analysis.
#' This function is geographically agnostic; it maps the user's selection depth
#' to standardized "level" columns (level1, level2, etc.), aggregates cases, 
#' and fills temporal gaps with 0 to ensure continuity.
#'
#' @param df A data frame containing raw data with standardized headers (level1, level2, etc.).
#' @param hierarchy_levels Character vector. The display names from config.json (e.g., c("Country", "Province")).
#' @param selected_level Character string. The specific level name selected in the UI.
#' @param col_country Character. Name of the country column. Default "country".
#' @param col_year Character. Name of the year column in the CSV. Default "year".
#' @param col_week Character. Name of the week column in the CSV. Default "week".
#' @param col_cases Character. Name of the case count column. Default "n_cases".
#'
#' @return A tibble with analysis_unit_id, n_cases, epi_date, and time_index.
#'
#' @importFrom dplyr group_by across all_of summarise mutate select arrange ungroup row_number filter rename
#' @importFrom tidyr complete unite
#' @importFrom ISOweek ISOweek2date
#' @export
prepare_weekly_data_geo <- function(df,
                                    hierarchy_levels,
                                    selected_level,
                                    col_country = "country",
                                    col_year    = "year",
                                    col_week    = "week",
                                    col_cases   = "n_cases") {
  
  # 1. Normalize column names to lowercase
  names(df) <- tolower(names(df))
  
  # 2. Determine Required Generic Columns
  # The function maps the depth of the selection to these standardized headers
  # If more levels are needed simply add to the list following this format
  generic_map <- c("country", "level1", "level2", "level3", "level4")
  
  # Find the depth index from the configuration vector
  level_depth <- which(hierarchy_levels == selected_level)
  
  if (length(level_depth) == 0) {
    stop("The selected level does not match any entry in the hierarchy configuration.")
  }
  
  # Identify only the columns needed for the current analysis depth
  required_cols <- generic_map[1:level_depth]
  
  # 3. Aggregate Existing Data
  df_agg <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(required_cols, col_year, col_week)))) %>%
    dplyr::summarise(
      n_cases = sum(.data[[col_cases]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(year = !!col_year, week = !!col_week)
  
  # 4. Gap Filling
  # Ensures a continuous sequence of weeks (1-52) exists for every analysis unit
  df_agg <- df_agg %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(required_cols))) %>%
    tidyr::complete(
      year = min(year):max(year),
      week = 1:52,
      fill = list(n_cases = 0)
    ) %>%
    # Remove future weeks in the final year that haven't occurred yet
    dplyr::filter(!(year == max(year) & week > max(df[[col_week]][df[[col_year]] == max(year)]))) %>%
    dplyr::ungroup()
  
  # 5. Create Analysis Unit ID and Time Index
  # Merges the geographic levels into a single ID and calculates a 1..N index
  df_agg <- df_agg %>%
    tidyr::unite("analysis_unit_id", dplyr::all_of(required_cols), sep = "|", remove = FALSE) %>%
    dplyr::mutate(
      iso_string = sprintf("%04d-W%02d-1", year, week),
      epi_date   = ISOweek::ISOweek2date(iso_string)
    ) %>%
    dplyr::select(-iso_string) %>%
    dplyr::arrange(analysis_unit_id, year, week) %>%
    dplyr::group_by(analysis_unit_id) %>%
    dplyr::mutate(time_index = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  return(df_agg)
}





#' @title Process, Deduplicate, and Fill Target Data
#'
#' @description
#' Filters the raw combined dataset to the user's requested locations and applies 
#' two critical safety checks: a null/empty check to prevent crashes during UI 
#' transitions, and a year range filter (2000-2100) to prevent 'Long Vector' 
#' memory errors caused by date typos. It then resolves overlapping weeks 
#' based on user preference and fills gaps via `prepare_weekly_data_geo`.
#'
#' @param raw_df Data frame. The combined raw data containing a 'file_index' column.
#' @param target_locations Character vector. The specific locations to keep.
#' @param target_col Character string. The column to filter locations on (e.g., "level2").
#' @param req_cols Character vector. The generic geographic columns required for grouping.
#' @param overlap_method Character string. How to resolve overlaps: "new", "old", or "sum".
#' @param hierarchy_levels Character vector. The display names from config.json.
#' @param selected_level Character string. The specific level name selected in the UI.
#'
#' @return A deduplicated, gap-filled tibble ready for CUSUM analysis, or NULL if input is invalid.
#'
#' @importFrom dplyr filter arrange group_by across all_of slice ungroup n summarise
#' @export
process_target_data <- function(raw_df, 
                                target_locations, 
                                target_col, 
                                req_cols, 
                                overlap_method, 
                                hierarchy_levels, 
                                selected_level) {
  
  # --- 1. Guard Clause: Stop if inputs are missing or empty ---
  # This prevents crashes when switching levels in the UI
  if (is.null(raw_df) || nrow(raw_df) == 0 || length(target_locations) == 0 || identical(target_locations, "")) {
    return(NULL)
  }
  
  # --- 2. Sanity Filter: Prevent 'Vector too long' error ---
  # Typo years like 20241012 would cause the gap-filler to crash R
  df_filtered <- raw_df %>%
    dplyr::filter(as.character(.data[[target_col]]) %in% as.character(target_locations)) %>%
    dplyr::filter(year >= 2000 & year <= 2100) 
  
  # Double-check we still have data after filtering
  if (nrow(df_filtered) == 0) return(NULL)
  
  # --- 3. Overlap Resolution (Deduplication) ---
  if (overlap_method == "old") {
    df_dedup <- df_filtered %>%
      dplyr::arrange(file_index) %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(c(req_cols, "year", "week")))) %>%
      dplyr::slice(1) %>%             
      dplyr::ungroup()
  } else if (overlap_method == "new") {
    df_dedup <- df_filtered %>%
      dplyr::arrange(file_index) %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(c(req_cols, "year", "week")))) %>%
      dplyr::slice(dplyr::n()) %>%   
      dplyr::ungroup()
  } else {
    # Default to "sum" if not old or new
    df_dedup <- df_filtered %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(req_cols, "year", "week")))) %>%
      dplyr::summarise(n_cases = sum(n_cases, na.rm = TRUE), .groups = "drop")
  }
  
  # --- 4. Gap Filling --- 
  # Now safe to run because year range is guaranteed
  filled_df <- prepare_weekly_data_geo(
    df               = df_dedup, 
    hierarchy_levels = hierarchy_levels,
    selected_level   = selected_level
  )
  
  return(filled_df)
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