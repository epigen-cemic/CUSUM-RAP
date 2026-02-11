library(lubridate)
library(jsonlite)
library(ISOweek)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rlang)
library(readr)
library(shiny)




source("R/functions_cusum.R")
source("R/functions_plot.R")
source("R/functions_io.R")
source("R/mod_cusum.R")




config_path <- here::here("www/config.json")
config <- jsonlite::fromJSON(config_path)


# Extract Argentina levels (currently fixed for argentina, can be dynamic in the future)
# This gives you: c("country", "province", "department", "fraction")
geo_choices<- config$Argentina$levels
names(geo_choices) <- tools::toTitleCase(geo_choices)