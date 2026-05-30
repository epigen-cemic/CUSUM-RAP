# Core Shiny & Web
library(shiny)
library(shinyjs)
library(DT)
library(jsonlite)

# Data Manipulation
library(lubridate)
library(ISOweek)
library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(readr)
library(stringr)

# Visualization
library(ggplot2)

# Utilities
library(here)

# Load shared configuration helpers before reading app-level settings.
source("R/shared/config_helpers.R")

# Load configuration before modules so shared helpers can use it.
config_path <- "config.json"
config <- jsonlite::fromJSON(config_path)

# App-level settings from config.json.
is_offline <- isTRUE(rap_config_value(config, "is_offline", FALSE))
active_countries <- rap_active_countries(config)
active_country <- rap_default_country(config)
cusum_config_messages <- rap_validate_cusum_config(config)

# Load the remaining modules/helpers.
app_files <- list.files(
  "R",
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = TRUE
)

app_files <- app_files[
  !grepl("^R/scripts/", app_files) &
    app_files != "R/shared/config_helpers.R"
]

invisible(lapply(sort(app_files), source))
