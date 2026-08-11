# CUSUM RAP Tool

The CUSUM RAP Tool is a Shiny application for early detection of unusual increases in weekly case counts by geographic unit.

## Project structure

```text
CUSUM-RAP/
  app.R
    Main Shiny entry point.

  global.R
    Loads required packages, reads configuration, and sources the application files.

  config.json
    Main configuration file. Defines the active country, country hierarchy, and
    CUSUM-specific validation settings.

  R/
    shared/
      Reusable helper functions used by the tool, including API-POP import,
      column standardisation, input validation, configuration helpers, UI helpers,
      file-management helpers, and preparation-log utilities.

    cusum/
      CUSUM-specific analytical code, parameter helpers, plotting functions,
      file/output helpers, and the main CUSUM Shiny module.

    scripts/
      Developer or maintenance scripts. These are not required during normal app
      execution and should not normally be sourced by `global.R`.

  www/
    Static app assets such as CSS, logos, images, icons, and documentation files
    served by Shiny.

  tests/
    Test runner and test files for validating import logic, configuration handling,
    CUSUM preparation, coverage validation, and core analytical helpers.

  README.md
    Project overview and local setup instructions.
```

## Recommended input

The app expects aggregated API-POP-style CSV files with one row per epidemiological week and geographic unit.

Minimum required columns for CUSUM:

- `year`
- `week`
- a geographic level column such as `country`, `level1`, `level2`, or `level3`
- `n_cases`

The importer accepts common aliases for case counts, including:

- `n_cases`
- `cases`
- `case_count`
- `count`
- `n`
- `events`
- `suspected_cases`

`population` is optional for the current count-based CUSUM workflow. It would only be required if the expected-frequency model is later changed to use population-adjusted rates.

## Key behavior

- Multiple CSV files can be uploaded.
- If uploaded files overlap by location/week, the user can choose how to resolve overlaps:
  - keep newest file information
  - keep oldest file information
  - sum overlapping counts
- Leaving the **Locations** field empty runs CUSUM for all available locations at the selected geographic level.
- The **Detection Period** controls the recent weeks monitored for alarms.
- Alerts are early warnings and require further epidemiological review.

## Main outputs

- **Overview:** outbreak heatmap across locations and weeks.
- **Detailed View:** observed counts, expected counts, and CUSUM process for a selected location.
- **Analysis Results:** downloadable CUSUM results.
- **Prepared Data:** cleaned, aggregated, gap-filled dataset used for the CUSUM analysis.

## Running locally

Open the project folder and run:

```r
shiny::runApp(".")
```

## Required R packages

The app requires the following R packages:

```r
install.packages(c(
  "shiny",
  "shinyjs",
  "DT",
  "htmltools",
  "jsonlite",
  "dplyr",
  "tidyr",
  "purrr",
  "rlang",
  "readr",
  "lubridate",
  "ISOweek",
  "stringr",
  "ggplot2",
  "here"
))
```



## Configuration

The active country is read from `config.json`:

```json
{
  "active_country": "Argentina"
}
```

The country block defines the geographic hierarchy used by the app.

## Notes for desktop packaging

This tool can be packaged as part of a local desktop application using Electron and a bundled R runtime. If Windows blocks the executable, users may need to allow the app through local security settings or contact IT support.


## Shared input handling

This tool now uses the shared `R/shared/api_pop_io.R` helper used across the RAP tools. CSV upload, delimiter detection, column alias resolution, type conversion, and validation are handled through the same API-POP import workflow used by the companion tool.

## Help and prepared-data audit trail

The app includes a Help tab with data requirements and links to English/Spanish documentation. The Prepared Data tab includes a data preparation log describing key transformations applied to the uploaded CSV before analysis, such as column standardisation, numeric conversion, filtering, overlap resolution, aggregation, and gap filling where applicable.

## Current configuration notes

The app-level settings at the top of `config.json` are:

- `is_offline`: retained for consistency across RAP tools. It is not currently required by the standard CUSUM workflow.
- `active_country`: controls which country configuration is used by default. This can be a single country string, for example `"Argentina"`, or a list such as `["Argentina", "England"]`. When more than one active country is configured, the app shows a Country dropdown.

CUSUM uses case counts over time and does not require population for the standard count-based workflow. Population is accepted if present, but is only needed if a future population-adjusted expected-frequency model is implemented.


## Running tests

The project includes a `testthat` test suite for the main helper functions and data-preparation workflow.

For detailed testing instructions, see [Testing](tests/TESTING.md).

Quick run from the project root:

```r
source("tests/testthat.R")
```

## CUSUM weekly coverage validation

The Prepared Data tab now distinguishes between uploaded observations and internally filled weeks. Missing week/location combinations are added to the prepared dataset with `0` cases so that each selected location has a continuous weekly time series. This does not modify the original uploaded CSV file.

The coverage checks are controlled from the `cusum` block in `config.json`:

```json
"cusum": {
  "default_detection_period": 52,
  "minimum_prepared_weeks": 52,
  "minimum_observed_coverage_stop": 0.40,
  "minimum_observed_coverage_warn": 0.70,
  "fill_missing_weeks_with_zero": true
}
```

Behavior:

- If the prepared weekly span is shorter than the selected detection period, CUSUM stops and asks for more data.
- If observed weekly coverage is below `minimum_observed_coverage_stop`, CUSUM stops because too much of the time series would be inferred as zero.
- If observed weekly coverage is between the stop and warning thresholds, CUSUM can run but shows a warning.
- If observed weekly coverage is above the warning threshold, CUSUM proceeds normally.

This prevents the tool from silently producing results from a file that only contains a small number of observed weeks spread across a longer period.


## CUSUM configuration validation

CUSUM-specific settings are stored in the `cusum` block of `config.json`:

- `default_detection_period`: default number of weeks used by the CUSUM analysis.
- `minimum_prepared_weeks`: minimum continuous prepared time-series length required before running CUSUM.
- `minimum_observed_coverage_stop`: observed-week coverage below this value stops the analysis.
- `minimum_observed_coverage_warn`: observed-week coverage below this value shows a warning.
- `fill_missing_weeks_with_zero`: controls whether missing location-week combinations are completed with zero cases in the prepared dataset.

At startup, the app validates these settings and shows a configuration warning if values are missing, outside their expected range, or internally inconsistent.

## UI consistency update

The standalone CUSUM and Geospatial RAP tools now use the same sidebar upload pattern: the CSV upload control appears directly in the left-hand input panel, and Help is available as a main output tab. The older top navigation links for Files, Selection, and external Help were removed from CUSUM to align the user experience between tools.

## Uploaded file manager

The upload panel includes a file manager for the current Shiny session. After files are uploaded, users can open **Manage files** to review uploaded files, deactivate files that should not be included in the analysis, remove inactive files, or clear all uploaded files without restarting the app.

All active uploaded files are combined for the CUSUM workflow. If more than one active file is present, the app asks how overlapping location/week records should be resolved.

## Project

**Project:** Analysis for Action  – Argentina Work Package WS5
**Institution / Delivery Partner:** CEMIC. 
The CUSUM-RAP was developed at CEMIC, the Argentina Delivery Partner for Analysis for Action (formerly the Pandemic Preparedness Toolkit), a project led by the UK Office for National Statistics (ONS). This work was supported by the Wellcome (Grant number 226596/Z/22/Z).

## Citation

If you use CUSUM-RAP in research, publications, or other work, please cite the software using the following DOI:
Giordano, M. E., Gili, J. A., López-Camelo, J. S.& Poletta, F. A. (2026). CUSUM-RAP: An early-warning tool for CUSUM-based outbreak detection in public health surveillance (Version v1.5.5) [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.21893968 

**CUSUM-RAP v1.5.5**  
https://doi.org/10.5281/zenodo.21893512

Citation metadata are also available in the `CITATION.cff` file.

