# Driver script for data workflow
#
# This calls the quarto (*.qmd) files that handle data processing for
# each step (raw data to L0, L0 to L1_normalize, etc).

begin_overall <- Sys.time()
library(quarto)

# Need to run this script from within pipeline directory
if(basename(getwd()) != "pipeline") {
    stop("Working directory needs to be pipeline/")
}
# If making a release, the repository should be clean
# Comment out the stop() if doing development work
git_status <- system2("git", "status", stdout = TRUE)
if(!any(grepl("clean", git_status))) {
   stop("Repository is not clean! Can't proceed to a release")
}

source("helpers.R")

# Settings ----------------------------------------------------

ROOT <- "./data_TEST"
VERSION <- "2-0"
RELEASE_DATE <- "2025-07-10"
# We use "Etc/GMT+5" rather than e.g. "America/New_York" because
# outputs should always be in STANDARD time
# See https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
L1_DATA_TZ <- "Etc/GMT+5"

# GitHub Actions settings -------------------------------------

if(Sys.getenv("CI") == "true") {
    ROOT <- "./data_TEST"
}

# Log file ----------------------------------------------------

LOGS <- file.path(ROOT, "Logs/")
now_string <- function() format(Sys.time(), "%Y%m%d.%H%M")

# Main logfile
LOGFILE <- file.path(LOGS, paste0("driver_log_", now_string(), ".txt"))
if(file.exists(LOGFILE)) file.remove(LOGFILE)

# Error handling ----------------------------------------------

STOP_ON_ERROR <- TRUE
ERROR_OCCURRED <- FALSE

# driver_try: ensure that if an *unexpected* error occurs,
# it's captured in the driver log file, and a flag is set
driver_try <- function(...) {
    tryCatch(eval(...),
             error = function(e) {
                 ERROR_OCCURRED <<- TRUE
                 log_warning("Driver: an error occurred!")
                 log_info(as.character(e))
                 if(STOP_ON_ERROR) stop(e)
             }
    )
}

# Construct L0 data ---------------------------------------------
# L0 data are raw but in long CSV form, with Logger/Table/ID columns added

message("Running L0")
new_section("Starting L0")

outfile <- paste0("L0_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

begin <- Sys.time()
driver_try(
    quarto_render("L0.qmd",
                  execute_params = list(DATA_ROOT = ROOT,
                                        html_outfile = outfile,
                                        logfile = LOGFILE,
                                        run_parallel = TRUE))
)
copy_output("L0.html", outfile)
L0_time <- format(round(difftime(Sys.time(), begin), 1))
message("L0 step: ", L0_time)

# 'Normalize' L0 data -------------------------------------------
# Matched with design_link info
# This is an intermediate step, not exposed to data users

message("Running L1_normalize.qmd")
new_section("Starting L1_normalize")

outfile <- paste0("L1_normalize_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

begin <- Sys.time()
driver_try(
    quarto_render("L1_normalize.qmd",
                  execute_params = list(DATA_ROOT = ROOT,
                                        L1_DATA_TIMEZONE = L1_DATA_TZ,
                                        html_outfile = outfile,
                                        logfile = LOGFILE,
                                        run_parallel = TRUE))
)
copy_output("L1_normalize.html", outfile)
L1_normalize_time <- format(round(difftime(Sys.time(), begin), 1))
message("L1_normalize step: ", L1_normalize_time)


# Construct L1 data --------------------------------------------
# This step drops unneeded columns, sorts, and adds extensive metadata
# Monthly files are written into folders based on site and year;
# see write_to_folders() in helpers.R

message("Running L1.qmd")
new_section("Starting L1")

outfile <- paste0("L1_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

begin <- Sys.time()
driver_try(
    quarto_render("L1.qmd",
                  execute_params = list(DATA_ROOT = ROOT,
                                        L1_VERSION = VERSION,
                                        L1_RELEASE_DATE = RELEASE_DATE,
                                        L1_DATA_TIMEZONE = L1_DATA_TZ,
                                        html_outfile = outfile,
                                        logfile = LOGFILE,
                                        run_parallel = FALSE))
)
copy_output("L1.html", outfile)
L1_time <- format(round(difftime(Sys.time(), begin), 1))
message("L1 step: ", L1_time)


# Manual QA/QC step ---------------------------------------------
# A MarineGEO-like Shiny app, allowing technicians to
# flag/annotate observations, would be applied to the output
# from this step


# Summary report data at L1 stage ------------------------------

# Seems like it? So that we move files out of this stage by hand
# (i.e. when month folder is complete)?


# Prep work for L2 data ----------------------------------------
# This is an intermediate step, not exposed to data users

message("Running L2_qaqc.qmd")
new_section("Starting L2_qaqc")

outfile <- paste0("L2_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

begin <- Sys.time()
driver_try(
    quarto_render("L2_qaqc.qmd",
              execute_params = list(DATA_ROOT = ROOT,
                                    L2_VERSION = VERSION,
                                    html_outfile = outfile))
)
copy_output("L2_qaqc.html", outfile)
L2_qaqc_time <- format(round(difftime(Sys.time(), begin), 1))
message("L2_qaqc step: ", L2_qaqc_time)

# Construct L2 data --------------------------------------------
# This step drops unneeded columns, sorts, and adds extensive metadata
# Annual files are written into folders based on site and year;
# see write_to_folders() in helpers.R

message("Running L2.qmd")
new_section("Starting L2")

outfile <- paste0("L2_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

begin <- Sys.time()
driver_try(
    quarto_render("L2.qmd",
                  execute_params = list(DATA_ROOT = ROOT,
                                        L2_VERSION = VERSION,
                                        L2_RELEASE_DATE = RELEASE_DATE,
                                        L2_DATA_TIMEZONE = L1_DATA_TZ,
                                        html_outfile = outfile,
                                        logfile = LOGFILE,
                                        run_parallel = FALSE))
)
copy_output("L2.html", outfile)
L2_time <- format(round(difftime(Sys.time(), begin), 1))
message("L2 step: ", L1_time)


if(ERROR_OCCURRED) warning ("One or more errors occurred!")

overall_time <- format(round(difftime(Sys.time(), begin_overall), 1))

message("\n-----------------")
message("L0:\t\t", L0_time)
message("L1_normalize:\t", L1_normalize_time)
message("L1:\t\t", L1_time)
message("L2_qaqc:\t", L2_qaqc_time)
message("L2:\t\t", L2_time)
message("Overall:\t", overall_time)
message("-----------------")
message("All done.")
