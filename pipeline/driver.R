# Driver script for data workflow
#
# This calls the quarto (*.qmd) files that handle data processing for
# each step (raw data to L0, L0 to L1_normalize, etc).

library(quarto)

# Need to run this script from within pipeline directory
if(basename(getwd()) != "pipeline") {
    stop("Working directory needs to be pipeline/")
}
if(!require("compasstools")) {
    stop("Need to remotes::install_github('COMPASS-DOE/compasstools')")
}

source("helpers.R")

# Settings ----------------------------------------------------

ROOT <- "./data_TEST"
VERSION <- "1-2"

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

driver_try(
    quarto_render("L0.qmd",
                  execute_params = list(DATA_ROOT = ROOT,
                                        html_outfile = outfile,
                                        logfile = LOGFILE,
                                        run_parallel = TRUE))
)
copy_output("L0.html", outfile)


# 'Normalize' L0 data -------------------------------------------
# Matched with design_link info
# This is an intermediate step, not exposed to data users

message("Running L1_normalize.qmd")
new_section("Starting L1_normalize")

outfile <- paste0("L1_normalize_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

driver_try(
    quarto_render("L1_normalize.qmd",
              execute_params = list(DATA_ROOT = ROOT,
                                    html_outfile = outfile,
                                    logfile = LOGFILE,
                                    run_parallel = TRUE))
)
copy_output("L1_normalize.html", outfile)


# Construct L1 data --------------------------------------------
# This step drops unneeded columns, sorts, and adds extensive metadata
# File are written into folders based on site, year, and month;
# see write_to_folders() in helpers.R

message("Running L1.qmd")
new_section("Starting L1")

outfile <- paste0("L1_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

driver_try(
    quarto_render("L1.qmd",
              execute_params = list(DATA_ROOT = ROOT,
                                    L1_VERSION = VERSION,
                                    html_outfile = outfile,
                                    logfile = LOGFILE,
                                    run_parallel = FALSE))
)
copy_output("L1.html", outfile)


# Manual QA/QC step ---------------------------------------------
# A MarineGEO-like Shiny app, allowing technicians to
# flag/annotate observations, would be applied to the output
# from this step


# Summary report data at L1 stage ------------------------------

# Seems like it? So that we move files out of this stage by hand
# (i.e. when month folder is complete)?


# Construct L2 data --------------------------------------------
# L2 data are semi-wide form, organized around experimental units for
# each timestamp. They have been matched with plot/experimental info, and ready for analysis

message("Running L2.qmd")
new_section("Starting L2")

outfile <- paste0("L2_", now_string(), ".html")
outfile <- file.path(LOGS, outfile)

# driver_try(
#     quarto_render("L2.qmd",
#               execute_params = list(DATA_ROOT = ROOT,
#                                     html_outfile = outfile,
#                                     logfile = LOGFILE,
#                                     run_parallel = TRUE))
# )
# copy_output("L2.html", outfile)


if(ERROR_OCCURRED) warning ("One or more errors occurred!")

message("All done.")
