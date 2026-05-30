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
  df <- api_pop_read_file(path)
  api_pop_standardise_columns(df, config = config, active_country = active_country, require_population = FALSE)
}

#' @title Standardise API-Pop Input Columns
#'
#' @description
#' Normalises common API-Pop column aliases to the names used internally by
#' the CUSUM pipeline. Population is accepted if present, but is not required
#' for the standard count-based CUSUM workflow.
#'
#' @param df Data frame read from one or more uploaded CSV files.
#'
#' @return A data frame with standard column names where possible.
#' @export
standardise_input_columns <- function(df) {
  api_pop_standardise_columns(df, config = config, active_country = active_country, require_population = FALSE)
}


#' @title Validate CUSUM Input Columns
#'
#' @description
#' Checks whether the uploaded API-Pop data contains the minimum fields needed
#' for CUSUM. CUSUM requires year, week, case counts, and the selected geography.
#' Population is optional unless a future rate-based expected-frequency model is used.
#'
#' @param df Data frame after \code{standardise_input_columns()}.
#' @param target_col Character. Selected geographic column, e.g. "level2".
#'
#' @return NULL when valid; otherwise a user-facing error message.
#' @export
validate_cusum_input <- function(df, target_col = NULL) {
  api_pop_validate_data(df, require_population = FALSE, target_col = target_col)
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


#' @title Read a CUSUM Configuration Value
#'
#' @description
#' Reads a value from the `cusum` block in `config.json`, returning a default
#' when the block or value is not present. The helper keeps CUSUM validation
#' thresholds configurable without hard-coding them in the server module.
#'
#' @param key Character scalar. Name of the CUSUM configuration entry.
#' @param default Value returned when the entry is missing.
#' @param cfg Parsed configuration list. Defaults to the global `config` object.
#'
#' @return The configured value or `default`.
#' @keywords internal
cusum_config_value <- function(key, default = NULL, cfg = config) {
  if (is.null(cfg$cusum) || is.null(cfg$cusum[[key]])) {
    return(default)
  }

  cfg$cusum[[key]]
}


#' @title Create a Human-Readable Percentage
#'
#' @param x Numeric proportion, for example `0.72`.
#'
#' @return Character scalar formatted as a percentage.
#' @keywords internal
cusum_percent <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("NA")
  }

  paste0(round(100 * x, 1), "%")
}


#' @title Convert ISO Year/Week Values to Dates Safely
#'
#' @description
#' Converts epidemiological year/week values to the Monday date of that ISO week.
#' Invalid combinations return `NA` instead of stopping the app.
#'
#' @param year Integer or numeric vector of epidemiological years.
#' @param week Integer or numeric vector of epidemiological weeks.
#'
#' @return Date vector.
#' @keywords internal
cusum_iso_week_to_date <- function(year, week) {
  iso_string <- sprintf("%04d-W%02d-1", as.integer(year), as.integer(week))

  as.Date(vapply(
    iso_string,
    function(x) {
      tryCatch(
        as.character(ISOweek::ISOweek2date(x)),
        error = function(e) NA_character_
      )
    },
    character(1)
  ))
}


#' @title Assess Prepared CUSUM Weekly Coverage
#'
#' @description
#' Checks whether the prepared weekly dataset has enough time span and enough
#' originally observed weeks to support CUSUM analysis. Missing weeks may be
#' filled with zero cases in the prepared dataset, but the coverage check prevents
#' the analysis from silently relying on mostly inferred zero-count weeks.
#'
#' @param prepared_df Prepared CUSUM data returned by `process_target_data()`.
#' @param detection_period Numeric. Number of weeks requested for CUSUM analysis.
#' @param cfg Parsed configuration list. Defaults to the global `config` object.
#'
#' @return A list with `status`, `message`, and threshold metadata. Status is one
#'   of `"ok"`, `"warn"`, or `"stop"`.
#' @keywords internal
cusum_assess_prepared_coverage <- function(prepared_df,
                                           detection_period = NULL,
                                           cfg = config) {
  if (is.null(prepared_df) || nrow(prepared_df) == 0) {
    return(list(
      status = "stop",
      message = "No prepared CUSUM data are available.",
      minimum_weeks = NA_integer_,
      stop_threshold = NA_real_,
      warn_threshold = NA_real_
    ))
  }

  coverage <- attr(prepared_df, "cusum_coverage_summary", exact = TRUE)

  if (is.null(coverage) || nrow(coverage) == 0) {
    return(list(
      status = "warn",
      message = "Prepared-data coverage could not be assessed.",
      minimum_weeks = NA_integer_,
      stop_threshold = NA_real_,
      warn_threshold = NA_real_
    ))
  }

  minimum_weeks <- as.integer(cusum_config_value("minimum_prepared_weeks", 52, cfg))
  if (!is.null(detection_period) && !is.na(detection_period)) {
    minimum_weeks <- max(minimum_weeks, as.integer(detection_period))
  }

  stop_threshold <- as.numeric(cusum_config_value("minimum_observed_coverage_stop", 0.40, cfg))
  warn_threshold <- as.numeric(cusum_config_value("minimum_observed_coverage_warn", 0.70, cfg))

  min_prepared_weeks <- min(coverage$prepared_weeks, na.rm = TRUE)
  min_coverage <- min(coverage$observed_coverage, na.rm = TRUE)
  total_observed <- sum(coverage$observed_weeks, na.rm = TRUE)
  total_prepared <- sum(coverage$prepared_weeks, na.rm = TRUE)
  overall_coverage <- if (total_prepared > 0) total_observed / total_prepared else NA_real_

  units_below_stop <- coverage$analysis_unit_id[
    coverage$observed_coverage < stop_threshold |
      coverage$prepared_weeks < minimum_weeks
  ]

  units_below_warn <- coverage$analysis_unit_id[
    coverage$observed_coverage < warn_threshold
  ]

  if (is.na(min_prepared_weeks) || min_prepared_weeks < minimum_weeks) {
    return(list(
      status = "stop",
      message = sprintf(
        "Insufficient data supplied. CUSUM requires at least %s prepared weeks for the selected detection period, but at least one selected location has only %s prepared week(s).",
        minimum_weeks,
        min_prepared_weeks
      ),
      minimum_weeks = minimum_weeks,
      stop_threshold = stop_threshold,
      warn_threshold = warn_threshold,
      overall_coverage = overall_coverage,
      minimum_coverage = min_coverage,
      units_below_threshold = units_below_stop
    ))
  }

  if (is.na(min_coverage) || min_coverage < stop_threshold) {
    return(list(
      status = "stop",
      message = sprintf(
        "Insufficient observed weekly data. The prepared period contains %s week(s), but the lowest observed coverage is %s. Missing weeks are filled with 0 cases only when they represent true zero-case weeks. Please upload a more complete weekly dataset.",
        total_prepared,
        cusum_percent(min_coverage)
      ),
      minimum_weeks = minimum_weeks,
      stop_threshold = stop_threshold,
      warn_threshold = warn_threshold,
      overall_coverage = overall_coverage,
      minimum_coverage = min_coverage,
      units_below_threshold = units_below_stop
    ))
  }

  if (min_coverage < warn_threshold) {
    return(list(
      status = "warn",
      message = sprintf(
        "Low observed weekly coverage. Overall coverage is %s and the lowest location coverage is %s. CUSUM can proceed, but results should be interpreted carefully if missing weeks do not represent true zero-case weeks.",
        cusum_percent(overall_coverage),
        cusum_percent(min_coverage)
      ),
      minimum_weeks = minimum_weeks,
      stop_threshold = stop_threshold,
      warn_threshold = warn_threshold,
      overall_coverage = overall_coverage,
      minimum_coverage = min_coverage,
      units_below_threshold = units_below_warn
    ))
  }

  list(
    status = "ok",
    message = sprintf(
      "Prepared-data coverage is sufficient. Overall observed coverage is %s; the lowest location coverage is %s.",
      cusum_percent(overall_coverage),
      cusum_percent(min_coverage)
    ),
    minimum_weeks = minimum_weeks,
    stop_threshold = stop_threshold,
    warn_threshold = warn_threshold,
    overall_coverage = overall_coverage,
    minimum_coverage = min_coverage,
    units_below_threshold = character(0)
  )
}


#' @title Prepare Weekly Data for Geographic Analysis
#'
#' @description
#' Standardizes raw data into a weekly time series format for CUSUM analysis.
#' The function aggregates cases at the selected geographic level, creates a
#' continuous weekly sequence for each analysis unit, fills missing weeks with
#' zero cases in the prepared dataset, and records observed-week coverage.
#'
#' @param df A data frame containing raw data with standardized headers.
#' @param hierarchy_levels Character vector. Display names from `config.json`.
#' @param selected_level Character string. Geographic level selected in the UI.
#' @param col_country Character. Name of the country column. Default `"country"`.
#' @param col_year Character. Name of the year column. Default `"year"`.
#' @param col_week Character. Name of the week column. Default `"week"`.
#' @param col_cases Character. Name of the case-count column. Default `"n_cases"`.
#'
#' @return A tibble with a continuous weekly time series and coverage metadata.
#' @importFrom dplyr group_by across all_of summarise mutate select arrange ungroup row_number filter n_distinct left_join
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

  names(df) <- tolower(names(df))

  generic_map <- c("country", "level1", "level2", "level3", "level4")
  level_depth <- which(hierarchy_levels == selected_level)

  if (length(level_depth) == 0) {
    stop("The selected level does not match any entry in the hierarchy configuration.")
  }

  required_cols <- generic_map[1:level_depth]

  df_grouped <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(required_cols, col_year, col_week))))

  if ("population" %in% names(df)) {
    df_agg <- df_grouped %>%
      dplyr::summarise(
        n_cases = sum(.data[[col_cases]], na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    df_agg <- df_grouped %>%
      dplyr::summarise(
        n_cases = sum(.data[[col_cases]], na.rm = TRUE),
        .groups = "drop"
      )
  }

  df_agg <- df_agg %>%
    dplyr::rename(year = !!col_year, week = !!col_week) %>%
    dplyr::mutate(
      epi_date = cusum_iso_week_to_date(year, week)
    ) %>%
    dplyr::filter(!is.na(epi_date))

  if (nrow(df_agg) == 0) {
    stop("No valid epidemiological year/week combinations were available after preparation.")
  }

  df_agg <- df_agg %>%
    tidyr::unite("analysis_unit_id", dplyr::all_of(required_cols), sep = "|", remove = FALSE)

  coverage_summary <- df_agg %>%
    dplyr::group_by(analysis_unit_id) %>%
    dplyr::summarise(
      first_epi_date = min(epi_date, na.rm = TRUE),
      last_epi_date = max(epi_date, na.rm = TRUE),
      observed_weeks = dplyr::n_distinct(epi_date),
      prepared_weeks = as.integer(difftime(max(epi_date, na.rm = TRUE), min(epi_date, na.rm = TRUE), units = "weeks")) + 1L,
      missing_weeks = pmax(prepared_weeks - observed_weeks, 0L),
      observed_coverage = ifelse(prepared_weeks > 0, observed_weeks / prepared_weeks, NA_real_),
      .groups = "drop"
    )

  df_filled <- df_agg %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(required_cols, "analysis_unit_id")))) %>%
    tidyr::complete(
      epi_date = seq(min(epi_date), max(epi_date), by = "week"),
      fill = list(n_cases = 0)
    ) %>%
    dplyr::arrange(analysis_unit_id, epi_date)

  if ("population" %in% names(df_filled)) {
    df_filled <- df_filled %>%
      dplyr::group_by(analysis_unit_id) %>%
      tidyr::fill(population, .direction = "downup") %>%
      dplyr::ungroup()
  }

  df_filled <- df_filled %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      year = lubridate::isoyear(epi_date),
      week = lubridate::isoweek(epi_date)
    ) %>%
    dplyr::arrange(analysis_unit_id, epi_date) %>%
    dplyr::group_by(analysis_unit_id) %>%
    dplyr::mutate(time_index = dplyr::row_number()) %>%
    dplyr::ungroup()

  attr(df_filled, "cusum_coverage_summary") <- coverage_summary

  df_filled
}


#' @title Process, Deduplicate, and Fill Target Data
#'
#' @description
#' Filters the raw combined dataset to the requested locations, resolves
#' overlapping location/week records, fills missing weekly periods with zero cases
#' in the prepared dataset, and records a coverage assessment. The original
#' uploaded data are not modified.
#'
#' @param raw_df Data frame. The combined raw data containing a `file_index` column.
#' @param target_locations Character vector. Specific locations to keep.
#' @param target_col Character string. Column to filter locations on.
#' @param req_cols Character vector. Generic geographic columns required for grouping.
#' @param overlap_method Character string. How to resolve overlaps: `"new"`, `"old"`, or `"sum"`.
#' @param hierarchy_levels Character vector. Display names from `config.json`.
#' @param selected_level Character string. Geographic level selected in the UI.
#'
#' @return A deduplicated, gap-filled tibble ready for CUSUM analysis, or `NULL`.
#' @importFrom dplyr filter arrange group_by across all_of slice ungroup n summarise
#' @export
process_target_data <- function(raw_df,
                                target_locations,
                                target_col,
                                req_cols,
                                overlap_method,
                                hierarchy_levels,
                                selected_level) {

  if (is.null(raw_df) || nrow(raw_df) == 0) {
    return(NULL)
  }

  original_rows <- nrow(raw_df)
  use_all_locations <- is.null(target_locations) || length(target_locations) == 0 || identical(target_locations, "")

  df_filtered <- raw_df %>%
    dplyr::filter(year >= 2000 & year <= 2100)

  invalid_year_rows <- original_rows - nrow(df_filtered)

  if (!use_all_locations) {
    df_filtered <- df_filtered %>%
      dplyr::filter(as.character(.data[[target_col]]) %in% as.character(target_locations))
  }

  if (nrow(df_filtered) == 0) return(NULL)

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
    df_grouped <- df_filtered %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(req_cols, "year", "week"))))

    if ("population" %in% names(df_filtered)) {
      df_dedup <- df_grouped %>%
        dplyr::summarise(
          n_cases = sum(n_cases, na.rm = TRUE),
          population = sum(population, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      df_dedup <- df_grouped %>%
        dplyr::summarise(n_cases = sum(n_cases, na.rm = TRUE), .groups = "drop")
    }
  }

  filled_df <- prepare_weekly_data_geo(
    df               = df_dedup,
    hierarchy_levels = hierarchy_levels,
    selected_level   = selected_level
  )

  coverage_summary <- attr(filled_df, "cusum_coverage_summary", exact = TRUE)
  total_missing <- if (is.null(coverage_summary)) NA_integer_ else sum(coverage_summary$missing_weeks, na.rm = TRUE)
  total_prepared <- if (is.null(coverage_summary)) nrow(filled_df) else sum(coverage_summary$prepared_weeks, na.rm = TRUE)
  total_observed <- if (is.null(coverage_summary)) NA_integer_ else sum(coverage_summary$observed_weeks, na.rm = TRUE)
  overall_coverage <- if (!is.na(total_observed) && total_prepared > 0) total_observed / total_prepared else NA_real_

  validation <- cusum_assess_prepared_coverage(filled_df)
  attr(filled_df, "cusum_validation") <- validation

  prep_log <- api_pop_get_log(raw_df)
  prep_log <- c(
    prep_log,
    sprintf("Started CUSUM preparation with %s uploaded row(s).", original_rows),
    if (invalid_year_rows > 0) sprintf("Removed %s row(s) with year values outside 2000-2100.", invalid_year_rows) else "All year values were within the accepted range 2000-2100.",
    if (use_all_locations) "No location filter was applied; all available locations were included." else sprintf("Filtered to %s selected location(s).", length(target_locations)),
    sprintf("Rows after year/location filtering: %s.", nrow(df_filtered)),
    sprintf("Overlap resolution method: %s.", overlap_method),
    sprintf("Rows after overlap resolution/aggregation: %s.", nrow(df_dedup)),
    "Created a continuous weekly time series for each selected analysis unit.",
    sprintf("Filled %s missing analysis-unit/week row(s) with 0 cases in the prepared dataset.", total_missing),
    sprintf("Prepared dataset contains %s row(s) across %s prepared week(s).", nrow(filled_df), total_prepared),
    sprintf("Observed weekly coverage before gap filling: %s.", cusum_percent(overall_coverage)),
    sprintf("Coverage status: %s. %s", toupper(validation$status), validation$message)
  )

  filled_df <- api_pop_set_log(filled_df, prep_log)
  attr(filled_df, "cusum_coverage_summary") <- coverage_summary
  attr(filled_df, "cusum_validation") <- validation

  filled_df
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