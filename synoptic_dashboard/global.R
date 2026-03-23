## This script holds functions and constants

library(googledrive)
library(janitor)
library(lubridate)
library(plotly)
library(purrr)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(parsedate)
library(DT)

source("aquatroll_functions.R")
source("sapflow_functions.R")
source("teros_functions.R")

site_locations <- read_csv("site_locations.csv")

theme_set(theme_minimal())

# First, set the GDrive folder to find files
directory <- "https://drive.google.com/drive/u/2/folders/1-C7KWbKBM9uUebEFs3Xsi-nZefjX6WOQ"

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = "*@pnnl.gov",
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "../synoptic_dashboard/.secrets"
)

# drive_auth(path = ".secrets/client_secret.json")
gdrive_files <- drive_ls(directory, recursive = FALSE, pattern = "^Compass.*\\.dat$")

# create the start-time based on two weeks ago (from today)
two_weeks_ago <- with_tz(Sys.time(), tzone = "EST") - days(14)


