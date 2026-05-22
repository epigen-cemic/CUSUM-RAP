# =============================================================================
# Configuration helpers
# -----------------------------------------------------------------------------
# Shared helpers for reading app-level settings from config.json. These helpers
# are intentionally small so both standalone tools can use the same behaviour.
# =============================================================================

#' Get a scalar value from config.json
#'
#' @description
#' Returns a value from the parsed configuration list, falling back to a default
#' when the key is absent or empty.
#'
#' @param config List created by `jsonlite::fromJSON()`.
#' @param key Character scalar. Name of the configuration entry to read.
#' @param default Value returned when the entry is missing or empty.
#'
#' @return The configured value or `default`.
#' @keywords internal
rap_config_value <- function(config, key, default = NULL) {
  value <- config[[key]]
  if (is.null(value) || length(value) == 0) {
    return(default)
  }
  value
}

#' List configured countries
#'
#' @description
#' Returns the names of configuration blocks that represent countries. App-level
#' keys such as `is_offline`, `active_country`, and `target_var` are excluded.
#'
#' @param config Parsed configuration list.
#'
#' @return Character vector of country keys available in the configuration.
#' @keywords internal
rap_available_countries <- function(config) {
  reserved <- c("is_offline", "active_country", "default_country", "target_var", "cusum", "Global")
  candidates <- setdiff(names(config), reserved)
  candidates[vapply(config[candidates], is.list, logical(1))]
}

#' Resolve active countries from config.json
#'
#' @description
#' Normalises the `active_country` setting to a character vector and removes
#' entries that do not exist as country blocks in the configuration.
#'
#' @param config Parsed configuration list.
#' @param default_country Character scalar used when no valid country is set.
#'
#' @return Character vector with one or more valid country keys.
#' @keywords internal
rap_active_countries <- function(config, default_country = "Argentina") {
  configured <- rap_config_value(config, "active_country", default_country)
  configured <- as.character(unlist(configured, use.names = FALSE))
  configured <- configured[configured %in% rap_available_countries(config)]

  if (length(configured) == 0 && default_country %in% rap_available_countries(config)) {
    configured <- default_country
  }

  if (length(configured) == 0) {
    configured <- rap_available_countries(config)[1]
  }

  unique(configured)
}

#' Get the default active country
#'
#' @param config Parsed configuration list.
#' @param default_country Character scalar used when no valid country is set.
#'
#' @return Character scalar containing the first active country.
#' @keywords internal
rap_default_country <- function(config, default_country = "Argentina") {
  rap_active_countries(config, default_country)[1]
}

#' Should the UI show a country selector?
#'
#' @param config Parsed configuration list.
#'
#' @return Logical. `TRUE` when more than one active country is configured.
#' @keywords internal
rap_show_country_selector <- function(config) {
  length(rap_active_countries(config)) > 1
}

#' Build CUSUM geographic choices
#'
#' @description
#' Creates a named vector for the CUSUM geographic-level selector from the
#' selected country's `levels` entry.
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#'
#' @return Named character vector suitable for `selectInput()`.
#' @keywords internal
rap_cusum_geo_choices <- function(config, country) {
  levels <- config[[country]]$levels
  levels <- as.character(levels)
  names(levels) <- tools::toTitleCase(levels)
  levels
}

#' Build geospatial level choices
#'
#' @description
#' Creates a named vector for the geospatial selector using configured mapping
#' entries. Display labels prefer `ui_label`, then `layer_name`, then the key.
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#' @param include_level1 Logical. If `FALSE`, only level2 and deeper are shown.
#'
#' @return Named character vector suitable for `selectInput()`.
#' @keywords internal
rap_geospatial_choices <- function(config, country, include_level1 = FALSE) {
  mappings <- config[[country]]$mapping
  if (is.null(mappings)) {
    return(character(0))
  }

  keys <- names(mappings)
  if (!include_level1) {
    keys <- keys[vapply(mappings[keys], function(x) {
      api_pop_mapping_column(x) %in% c("level2", "level3", "level4")
    }, logical(1))]
  }

  choices <- keys
  names(choices) <- vapply(keys, function(k) {
    cfg <- mappings[[k]]
    if (!is.null(cfg$ui_label)) return(cfg$ui_label)
    if (!is.null(cfg$layer_name)) return(cfg$layer_name)
    k
  }, character(1))

  choices
}

#' Get the configured Geopackage path for a country
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#' @param data_dir Character scalar. Folder containing Geopackage files.
#'
#' @return Character path to the configured Geopackage.
#' @keywords internal
rap_gpkg_path <- function(config, country, data_dir = "raw_data") {
  file_name <- config[[country]]$file_name
  if (is.null(file_name) || is.na(file_name) || !nzchar(file_name)) {
    return(NA_character_)
  }
  file.path(data_dir, file_name)
}

#' Check whether a country's Geopackage exists
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#' @param data_dir Character scalar. Folder containing Geopackage files.
#'
#' @return Logical indicating whether the configured Geopackage exists.
#' @keywords internal
rap_has_gpkg <- function(config, country, data_dir = "raw_data") {
  path <- rap_gpkg_path(config, country, data_dir)
  is.character(path) && length(path) == 1 && !is.na(path) && file.exists(path)
}

#' Build a user-facing missing-Geopackage message
#'
#' @param config Parsed configuration list.
#' @param country Character scalar. Country key to read.
#' @param data_dir Character scalar. Folder containing Geopackage files.
#'
#' @return Character message explaining which file is missing.
#' @keywords internal
rap_missing_gpkg_message <- function(config, country, data_dir = "raw_data") {
  path <- rap_gpkg_path(config, country, data_dir)
  sprintf(
    "Geographic data are not available for '%s'. Expected Geopackage: %s. Please add the file or select another configured country.",
    country,
    path
  )
}


#' Validate CUSUM configuration settings
#'
#' @description
#' Checks the CUSUM settings in `config.json` and returns user-facing messages
#' for missing or invalid values. The validation is intentionally conservative:
#' invalid values are reported to the user but the app can still open so the
#' configuration can be inspected and corrected.
#'
#' @param config Parsed configuration list.
#'
#' @return Character vector of configuration warnings. Empty when no issues are found.
#' @keywords internal
rap_validate_cusum_config <- function(config) {
  messages <- character(0)
  cfg <- config$cusum

  if (is.null(cfg) || !is.list(cfg)) {
    return("config.json is missing the 'cusum' settings block.")
  }

  numeric_checks <- list(
    default_detection_period = c(min = 1, max = Inf),
    minimum_prepared_weeks = c(min = 1, max = Inf),
    minimum_observed_coverage_stop = c(min = 0, max = 1),
    minimum_observed_coverage_warn = c(min = 0, max = 1)
  )

  for (key in names(numeric_checks)) {
    value <- suppressWarnings(as.numeric(cfg[[key]]))
    bounds <- numeric_checks[[key]]
    if (length(value) != 1 || is.na(value) || value < bounds[["min"]] || value > bounds[["max"]]) {
      messages <- c(messages, sprintf("CUSUM config setting '%s' is missing or outside the expected range.", key))
    }
  }

  stop_threshold <- suppressWarnings(as.numeric(cfg$minimum_observed_coverage_stop))
  warn_threshold <- suppressWarnings(as.numeric(cfg$minimum_observed_coverage_warn))
  if (!is.na(stop_threshold) && !is.na(warn_threshold) && stop_threshold > warn_threshold) {
    messages <- c(messages, "CUSUM config has minimum_observed_coverage_stop greater than minimum_observed_coverage_warn.")
  }

  fill_missing <- cfg$fill_missing_weeks_with_zero
  if (!is.logical(fill_missing) || length(fill_missing) != 1) {
    messages <- c(messages, "CUSUM config setting 'fill_missing_weeks_with_zero' should be true or false.")
  }

  unique(messages)
}
