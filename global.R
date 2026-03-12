# Core Shiny & Web
library(jsonlite)
library(shinyjs)
library(shiny)
library(DT)   


# Data Manipulation
library(lubridate)
library(ISOweek)
library(dplyr)
library(tidyr)
library(purrr)
library(rlang)
library(readr)


# Visualization
library(ggplot2)

# Utilities
library(here) 


source("R/functions_parameters.R")
source("R/functions_cusum.R")
source("R/functions_plot.R")
source("R/functions_io.R")
source("R/mod_cusum.R")




config_path <- here::here("config.json")
config <- jsonlite::fromJSON(config_path)


# Extract the desired levels from a country or a selection of countries
geo_choices<- config$Argentina$levels
names(geo_choices) <- tools::toTitleCase(geo_choices)